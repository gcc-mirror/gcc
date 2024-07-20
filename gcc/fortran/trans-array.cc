/* Array translation routines
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

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

/* trans-array.cc-- Various array related code, including scalarization,
                   allocation, initialization and other support routines.  */

/* How the scalarizer works.
   In gfortran, array expressions use the same core routines as scalar
   expressions.
   First, a Scalarization State (SS) chain is built.  This is done by walking
   the expression tree, and building a linear list of the terms in the
   expression.  As the tree is walked, scalar subexpressions are translated.

   The scalarization parameters are stored in a gfc_loopinfo structure.
   First the start and stride of each term is calculated by
   gfc_conv_ss_startstride.  During this process the expressions for the array
   descriptors and data pointers are also translated.

   If the expression is an assignment, we must then resolve any dependencies.
   In Fortran all the rhs values of an assignment must be evaluated before
   any assignments take place.  This can require a temporary array to store the
   values.  We also require a temporary when we are passing array expressions
   or vector subscripts as procedure parameters.

   Array sections are passed without copying to a temporary.  These use the
   scalarizer to determine the shape of the section.  The flag
   loop->array_parameter tells the scalarizer that the actual values and loop
   variables will not be required.

   The function gfc_conv_loop_setup generates the scalarization setup code.
   It determines the range of the scalarizing loop variables.  If a temporary
   is required, this is created and initialized.  Code for scalar expressions
   taken outside the loop is also generated at this time.  Next the offset and
   scaling required to translate from loop variables to array indices for each
   term is calculated.

   A call to gfc_start_scalarized_body marks the start of the scalarized
   expression.  This creates a scope and declares the loop variables.  Before
   calling this gfc_make_ss_chain_used must be used to indicate which terms
   will be used inside this loop.

   The scalar gfc_conv_* functions are then used to build the main body of the
   scalarization loop.  Scalarization loop variables and precalculated scalar
   values are automatically substituted.  Note that gfc_advance_se_ss_chain
   must be used, rather than changing the se->ss directly.

   For assignment expressions requiring a temporary two sub loops are
   generated.  The first stores the result of the expression in the temporary,
   the second copies it to the result.  A call to
   gfc_trans_scalarized_loop_boundary marks the end of the main loop code and
   the start of the copying loop.  The temporary may be less than full rank.

   Finally gfc_trans_scalarizing_loops is called to generate the implicit do
   loops.  The loops are added to the pre chain of the loopinfo.  The post
   chain may still contain cleanup code.

   After the loop code has been added into its parent scope gfc_cleanup_loop
   is called to free all the SS allocated by the scalarizer.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "tree.h"
#include "gfortran.h"
#include "gimple-expr.h"
#include "tree-iterator.h"
#include "stringpool.h"  /* Required by "attribs.h".  */
#include "attribs.h" /* For lookup_attribute.  */
#include "trans.h"
#include "fold-const.h"
#include "constructor.h"
#include "trans-types.h"
#include "trans-array.h"
#include "trans-const.h"
#include "dependency.h"

static bool gfc_get_array_constructor_size (mpz_t *, gfc_constructor_base);

/* The contents of this structure aren't actually used, just the address.  */
static gfc_ss gfc_ss_terminator_var;
gfc_ss * const gfc_ss_terminator = &gfc_ss_terminator_var;


static tree
gfc_array_dataptr_type (tree desc)
{
  return (GFC_TYPE_ARRAY_DATAPTR_TYPE (TREE_TYPE (desc)));
}

/* Build expressions to access members of the CFI descriptor.  */
#define CFI_FIELD_BASE_ADDR 0
#define CFI_FIELD_ELEM_LEN 1
#define CFI_FIELD_VERSION 2
#define CFI_FIELD_RANK 3
#define CFI_FIELD_ATTRIBUTE 4
#define CFI_FIELD_TYPE 5
#define CFI_FIELD_DIM 6

#define CFI_DIM_FIELD_LOWER_BOUND 0
#define CFI_DIM_FIELD_EXTENT 1
#define CFI_DIM_FIELD_SM 2

static tree
gfc_get_cfi_descriptor_field (tree desc, unsigned field_idx)
{
  tree type = TREE_TYPE (desc);
  gcc_assert (TREE_CODE (type) == RECORD_TYPE
	      && TYPE_FIELDS (type)
	      && (strcmp ("base_addr",
			 IDENTIFIER_POINTER (DECL_NAME (TYPE_FIELDS (type))))
		  == 0));
  tree field = gfc_advance_chain (TYPE_FIELDS (type), field_idx);
  gcc_assert (field != NULL_TREE);

  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			  desc, field, NULL_TREE);
}

tree
gfc_get_cfi_desc_base_addr (tree desc)
{
  return gfc_get_cfi_descriptor_field (desc, CFI_FIELD_BASE_ADDR);
}

tree
gfc_get_cfi_desc_elem_len (tree desc)
{
  return gfc_get_cfi_descriptor_field (desc, CFI_FIELD_ELEM_LEN);
}

tree
gfc_get_cfi_desc_version (tree desc)
{
  return gfc_get_cfi_descriptor_field (desc, CFI_FIELD_VERSION);
}

tree
gfc_get_cfi_desc_rank (tree desc)
{
  return gfc_get_cfi_descriptor_field (desc, CFI_FIELD_RANK);
}

tree
gfc_get_cfi_desc_type (tree desc)
{
  return gfc_get_cfi_descriptor_field (desc, CFI_FIELD_TYPE);
}

tree
gfc_get_cfi_desc_attribute (tree desc)
{
  return gfc_get_cfi_descriptor_field (desc, CFI_FIELD_ATTRIBUTE);
}

static tree
gfc_get_cfi_dim_item (tree desc, tree idx, unsigned field_idx)
{
  tree tmp = gfc_get_cfi_descriptor_field (desc, CFI_FIELD_DIM);
  tmp = gfc_build_array_ref (tmp, idx, NULL_TREE, true);
  tree field = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (tmp)), field_idx);
  gcc_assert (field != NULL_TREE);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			  tmp, field, NULL_TREE);
}

tree
gfc_get_cfi_dim_lbound (tree desc, tree idx)
{
  return gfc_get_cfi_dim_item (desc, idx, CFI_DIM_FIELD_LOWER_BOUND);
}

tree
gfc_get_cfi_dim_extent (tree desc, tree idx)
{
  return gfc_get_cfi_dim_item (desc, idx, CFI_DIM_FIELD_EXTENT);
}

tree
gfc_get_cfi_dim_sm (tree desc, tree idx)
{
  return gfc_get_cfi_dim_item (desc, idx, CFI_DIM_FIELD_SM);
}

#undef CFI_FIELD_BASE_ADDR
#undef CFI_FIELD_ELEM_LEN
#undef CFI_FIELD_VERSION
#undef CFI_FIELD_RANK
#undef CFI_FIELD_ATTRIBUTE
#undef CFI_FIELD_TYPE
#undef CFI_FIELD_DIM

#undef CFI_DIM_FIELD_LOWER_BOUND
#undef CFI_DIM_FIELD_EXTENT
#undef CFI_DIM_FIELD_SM

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

static tree
gfc_get_descriptor_field (tree desc, unsigned field_idx)
{
  tree type = TREE_TYPE (desc);
  gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));

  tree field = gfc_advance_chain (TYPE_FIELDS (type), field_idx);
  gcc_assert (field != NULL_TREE);

  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			  desc, field, NULL_TREE);
}

/* This provides READ-ONLY access to the data field.  The field itself
   doesn't have the proper type.  */

tree
gfc_conv_descriptor_data_get (tree desc)
{
  tree type = TREE_TYPE (desc);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    gcc_unreachable ();

  tree field = gfc_get_descriptor_field (desc, DATA_FIELD);
  return fold_convert (GFC_TYPE_ARRAY_DATAPTR_TYPE (type), field);
}

/* This provides WRITE access to the data field.

   TUPLES_P is true if we are generating tuples.

   This function gets called through the following macros:
     gfc_conv_descriptor_data_set
     gfc_conv_descriptor_data_set.  */

void
gfc_conv_descriptor_data_set (stmtblock_t *block, tree desc, tree value)
{
  tree field = gfc_get_descriptor_field (desc, DATA_FIELD);
  gfc_add_modify (block, field, fold_convert (TREE_TYPE (field), value));
}


/* This provides address access to the data field.  This should only be
   used by array allocation, passing this on to the runtime.  */

tree
gfc_conv_descriptor_data_addr (tree desc)
{
  tree field = gfc_get_descriptor_field (desc, DATA_FIELD);
  return gfc_build_addr_expr (NULL_TREE, field);
}

static tree
gfc_conv_descriptor_offset (tree desc)
{
  tree field = gfc_get_descriptor_field (desc, OFFSET_FIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

tree
gfc_conv_descriptor_offset_get (tree desc)
{
  return gfc_conv_descriptor_offset (desc);
}

void
gfc_conv_descriptor_offset_set (stmtblock_t *block, tree desc,
				tree value)
{
  tree t = gfc_conv_descriptor_offset (desc);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
}


tree
gfc_conv_descriptor_dtype (tree desc)
{
  tree field = gfc_get_descriptor_field (desc, DTYPE_FIELD);
  gcc_assert (TREE_TYPE (field) == get_dtype_type_node ());
  return field;
}

static tree
gfc_conv_descriptor_span (tree desc)
{
  tree field = gfc_get_descriptor_field (desc, SPAN_FIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

tree
gfc_conv_descriptor_span_get (tree desc)
{
  return gfc_conv_descriptor_span (desc);
}

void
gfc_conv_descriptor_span_set (stmtblock_t *block, tree desc,
				tree value)
{
  tree t = gfc_conv_descriptor_span (desc);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
}


tree
gfc_conv_descriptor_rank (tree desc)
{
  tree tmp;
  tree dtype;

  dtype = gfc_conv_descriptor_dtype (desc);
  tmp = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (dtype)), GFC_DTYPE_RANK);
  gcc_assert (tmp != NULL_TREE
	      && TREE_TYPE (tmp) == signed_char_type_node);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			  dtype, tmp, NULL_TREE);
}


tree
gfc_conv_descriptor_version (tree desc)
{
  tree tmp;
  tree dtype;

  dtype = gfc_conv_descriptor_dtype (desc);
  tmp = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (dtype)), GFC_DTYPE_VERSION);
  gcc_assert (tmp != NULL_TREE
	      && TREE_TYPE (tmp) == integer_type_node);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			  dtype, tmp, NULL_TREE);
}


/* Return the element length from the descriptor dtype field.  */

tree
gfc_conv_descriptor_elem_len (tree desc)
{
  tree tmp;
  tree dtype;

  dtype = gfc_conv_descriptor_dtype (desc);
  tmp = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (dtype)),
			   GFC_DTYPE_ELEM_LEN);
  gcc_assert (tmp != NULL_TREE
	      && TREE_TYPE (tmp) == size_type_node);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			  dtype, tmp, NULL_TREE);
}


tree
gfc_conv_descriptor_attribute (tree desc)
{
  tree tmp;
  tree dtype;

  dtype = gfc_conv_descriptor_dtype (desc);
  tmp = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (dtype)),
			   GFC_DTYPE_ATTRIBUTE);
  gcc_assert (tmp!= NULL_TREE
	      && TREE_TYPE (tmp) == short_integer_type_node);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			  dtype, tmp, NULL_TREE);
}

tree
gfc_conv_descriptor_type (tree desc)
{
  tree tmp;
  tree dtype;

  dtype = gfc_conv_descriptor_dtype (desc);
  tmp = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (dtype)), GFC_DTYPE_TYPE);
  gcc_assert (tmp!= NULL_TREE
	      && TREE_TYPE (tmp) == signed_char_type_node);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			  dtype, tmp, NULL_TREE);
}

tree
gfc_get_descriptor_dimension (tree desc)
{
  tree field = gfc_get_descriptor_field (desc, DIMENSION_FIELD);
  gcc_assert (TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (field))) == RECORD_TYPE);
  return field;
}


static tree
gfc_conv_descriptor_dimension (tree desc, tree dim)
{
  tree tmp;

  tmp = gfc_get_descriptor_dimension (desc);

  return gfc_build_array_ref (tmp, dim, NULL_TREE, true);
}


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

static tree
gfc_conv_descriptor_subfield (tree desc, tree dim, unsigned field_idx)
{
  tree tmp = gfc_conv_descriptor_dimension (desc, dim);
  tree field = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (tmp)), field_idx);
  gcc_assert (field != NULL_TREE);

  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			  tmp, field, NULL_TREE);
}

static tree
gfc_conv_descriptor_stride (tree desc, tree dim)
{
  tree field = gfc_conv_descriptor_subfield (desc, dim, STRIDE_SUBFIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

tree
gfc_conv_descriptor_stride_get (tree desc, tree dim)
{
  tree type = TREE_TYPE (desc);
  gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));
  if (integer_zerop (dim)
      && (GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ALLOCATABLE
	  ||GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_SHAPE_CONT
	  ||GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_RANK_CONT
	  ||GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_POINTER_CONT))
    return gfc_index_one_node;

  return gfc_conv_descriptor_stride (desc, dim);
}

void
gfc_conv_descriptor_stride_set (stmtblock_t *block, tree desc,
				tree dim, tree value)
{
  tree t = gfc_conv_descriptor_stride (desc, dim);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
}

static tree
gfc_conv_descriptor_lbound (tree desc, tree dim)
{
  tree field = gfc_conv_descriptor_subfield (desc, dim, LBOUND_SUBFIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

tree
gfc_conv_descriptor_lbound_get (tree desc, tree dim)
{
  return gfc_conv_descriptor_lbound (desc, dim);
}

void
gfc_conv_descriptor_lbound_set (stmtblock_t *block, tree desc,
				tree dim, tree value)
{
  tree t = gfc_conv_descriptor_lbound (desc, dim);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
}

static tree
gfc_conv_descriptor_ubound (tree desc, tree dim)
{
  tree field = gfc_conv_descriptor_subfield (desc, dim, UBOUND_SUBFIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

tree
gfc_conv_descriptor_ubound_get (tree desc, tree dim)
{
  return gfc_conv_descriptor_ubound (desc, dim);
}

void
gfc_conv_descriptor_ubound_set (stmtblock_t *block, tree desc,
				tree dim, tree value)
{
  tree t = gfc_conv_descriptor_ubound (desc, dim);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
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


/* Obtain offsets for trans-types.cc(gfc_get_array_descr_info).  */

void
gfc_get_descriptor_offsets_for_info (const_tree desc_type, tree *data_off,
				     tree *dtype_off, tree *span_off,
				     tree *dim_off, tree *dim_size,
				     tree *stride_suboff, tree *lower_suboff,
				     tree *upper_suboff)
{
  tree field;
  tree type;

  type = TYPE_MAIN_VARIANT (desc_type);
  field = gfc_advance_chain (TYPE_FIELDS (type), DATA_FIELD);
  *data_off = byte_position (field);
  field = gfc_advance_chain (TYPE_FIELDS (type), DTYPE_FIELD);
  *dtype_off = byte_position (field);
  field = gfc_advance_chain (TYPE_FIELDS (type), SPAN_FIELD);
  *span_off = byte_position (field);
  field = gfc_advance_chain (TYPE_FIELDS (type), DIMENSION_FIELD);
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


/* Mark a SS chain as used.  Flags specifies in which loops the SS is used.
   flags & 1 = Main loop body.
   flags & 2 = temp copy loop.  */

void
gfc_mark_ss_chain_used (gfc_ss * ss, unsigned flags)
{
  for (; ss != gfc_ss_terminator; ss = ss->next)
    ss->info->useflags = flags;
}


/* Free a gfc_ss chain.  */

void
gfc_free_ss_chain (gfc_ss * ss)
{
  gfc_ss *next;

  while (ss != gfc_ss_terminator)
    {
      gcc_assert (ss != NULL);
      next = ss->next;
      gfc_free_ss (ss);
      ss = next;
    }
}


static void
free_ss_info (gfc_ss_info *ss_info)
{
  int n;

  ss_info->refcount--;
  if (ss_info->refcount > 0)
    return;

  gcc_assert (ss_info->refcount == 0);

  switch (ss_info->type)
    {
    case GFC_SS_SECTION:
      for (n = 0; n < GFC_MAX_DIMENSIONS; n++)
	if (ss_info->data.array.subscript[n])
	  gfc_free_ss_chain (ss_info->data.array.subscript[n]);
      break;

    default:
      break;
    }

  free (ss_info);
}


/* Free a SS.  */

void
gfc_free_ss (gfc_ss * ss)
{
  free_ss_info (ss->info);
  free (ss);
}


/* Creates and initializes an array type gfc_ss struct.  */

gfc_ss *
gfc_get_array_ss (gfc_ss *next, gfc_expr *expr, int dimen, gfc_ss_type type)
{
  gfc_ss *ss;
  gfc_ss_info *ss_info;
  int i;

  ss_info = gfc_get_ss_info ();
  ss_info->refcount++;
  ss_info->type = type;
  ss_info->expr = expr;

  ss = gfc_get_ss ();
  ss->info = ss_info;
  ss->next = next;
  ss->dimen = dimen;
  for (i = 0; i < ss->dimen; i++)
    ss->dim[i] = i;

  return ss;
}


/* Creates and initializes a temporary type gfc_ss struct.  */

gfc_ss *
gfc_get_temp_ss (tree type, tree string_length, int dimen)
{
  gfc_ss *ss;
  gfc_ss_info *ss_info;
  int i;

  ss_info = gfc_get_ss_info ();
  ss_info->refcount++;
  ss_info->type = GFC_SS_TEMP;
  ss_info->string_length = string_length;
  ss_info->data.temp.type = type;

  ss = gfc_get_ss ();
  ss->info = ss_info;
  ss->next = gfc_ss_terminator;
  ss->dimen = dimen;
  for (i = 0; i < ss->dimen; i++)
    ss->dim[i] = i;

  return ss;
}


/* Creates and initializes a scalar type gfc_ss struct.  */

gfc_ss *
gfc_get_scalar_ss (gfc_ss *next, gfc_expr *expr)
{
  gfc_ss *ss;
  gfc_ss_info *ss_info;

  ss_info = gfc_get_ss_info ();
  ss_info->refcount++;
  ss_info->type = GFC_SS_SCALAR;
  ss_info->expr = expr;

  ss = gfc_get_ss ();
  ss->info = ss_info;
  ss->next = next;

  return ss;
}


/* Free all the SS associated with a loop.  */

void
gfc_cleanup_loop (gfc_loopinfo * loop)
{
  gfc_loopinfo *loop_next, **ploop;
  gfc_ss *ss;
  gfc_ss *next;

  ss = loop->ss;
  while (ss != gfc_ss_terminator)
    {
      gcc_assert (ss != NULL);
      next = ss->loop_chain;
      gfc_free_ss (ss);
      ss = next;
    }

  /* Remove reference to self in the parent loop.  */
  if (loop->parent)
    for (ploop = &loop->parent->nested; *ploop; ploop = &(*ploop)->next)
      if (*ploop == loop)
	{
	  *ploop = loop->next;
	  break;
	}

  /* Free non-freed nested loops.  */
  for (loop = loop->nested; loop; loop = loop_next)
    {
      loop_next = loop->next;
      gfc_cleanup_loop (loop);
      free (loop);
    }
}


static void
set_ss_loop (gfc_ss *ss, gfc_loopinfo *loop)
{
  int n;

  for (; ss != gfc_ss_terminator; ss = ss->next)
    {
      ss->loop = loop;

      if (ss->info->type == GFC_SS_SCALAR
	  || ss->info->type == GFC_SS_REFERENCE
	  || ss->info->type == GFC_SS_TEMP)
	continue;

      for (n = 0; n < GFC_MAX_DIMENSIONS; n++)
	if (ss->info->data.array.subscript[n] != NULL)
	  set_ss_loop (ss->info->data.array.subscript[n], loop);
    }
}


/* Associate a SS chain with a loop.  */

void
gfc_add_ss_to_loop (gfc_loopinfo * loop, gfc_ss * head)
{
  gfc_ss *ss;
  gfc_loopinfo *nested_loop;

  if (head == gfc_ss_terminator)
    return;

  set_ss_loop (head, loop);

  ss = head;
  for (; ss && ss != gfc_ss_terminator; ss = ss->next)
    {
      if (ss->nested_ss)
	{
	  nested_loop = ss->nested_ss->loop;

	  /* More than one ss can belong to the same loop.  Hence, we add the
	     loop to the chain only if it is different from the previously
	     added one, to avoid duplicate nested loops.  */
	  if (nested_loop != loop->nested)
	    {
	      gcc_assert (nested_loop->parent == NULL);
	      nested_loop->parent = loop;

	      gcc_assert (nested_loop->next == NULL);
	      nested_loop->next = loop->nested;
	      loop->nested = nested_loop;
	    }
	  else
	    gcc_assert (nested_loop->parent == loop);
	}

      if (ss->next == gfc_ss_terminator)
	ss->loop_chain = loop->ss;
      else
	ss->loop_chain = ss->next;
    }
  gcc_assert (ss == gfc_ss_terminator);
  loop->ss = head;
}


/* Returns true if the expression is an array pointer.  */

static bool
is_pointer_array (tree expr)
{
  if (expr == NULL_TREE
      || !GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (expr))
      || GFC_CLASS_TYPE_P (TREE_TYPE (expr)))
    return false;

  if (VAR_P (expr)
      && GFC_DECL_PTR_ARRAY_P (expr))
    return true;

  if (TREE_CODE (expr) == PARM_DECL
      && GFC_DECL_PTR_ARRAY_P (expr))
    return true;

  if (INDIRECT_REF_P (expr)
      && GFC_DECL_PTR_ARRAY_P (TREE_OPERAND (expr, 0)))
    return true;

  /* The field declaration is marked as an pointer array.  */
  if (TREE_CODE (expr) == COMPONENT_REF
      && GFC_DECL_PTR_ARRAY_P (TREE_OPERAND (expr, 1))
      && !GFC_CLASS_TYPE_P (TREE_TYPE (TREE_OPERAND (expr, 1))))
    return true;

  return false;
}


/* If the symbol or expression reference a CFI descriptor, return the
   pointer to the converted gfc descriptor. If an array reference is
   present as the last argument, check that it is the one applied to
   the CFI descriptor in the expression. Note that the CFI object is
   always the symbol in the expression!  */

static bool
get_CFI_desc (gfc_symbol *sym, gfc_expr *expr,
	      tree *desc, gfc_array_ref *ar)
{
  tree tmp;

  if (!is_CFI_desc (sym, expr))
    return false;

  if (expr && ar)
    {
      if (!(expr->ref && expr->ref->type == REF_ARRAY)
	  || (&expr->ref->u.ar != ar))
	return false;
    }

  if (sym == NULL)
    tmp = expr->symtree->n.sym->backend_decl;
  else
    tmp = sym->backend_decl;

  if (tmp && DECL_LANG_SPECIFIC (tmp) && GFC_DECL_SAVED_DESCRIPTOR (tmp))
    tmp = GFC_DECL_SAVED_DESCRIPTOR (tmp);

  *desc = tmp;
  return true;
}


/* Return the span of an array.  */

tree
gfc_get_array_span (tree desc, gfc_expr *expr)
{
  tree tmp;

  if (is_pointer_array (desc)
      || (get_CFI_desc (NULL, expr, &desc, NULL)
	  && (POINTER_TYPE_P (TREE_TYPE (desc))
	      ? GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (TREE_TYPE (desc)))
	      : GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))))
    {
      if (POINTER_TYPE_P (TREE_TYPE (desc)))
	desc = build_fold_indirect_ref_loc (input_location, desc);

      /* This will have the span field set.  */
      tmp = gfc_conv_descriptor_span_get (desc);
    }
  else if (expr->ts.type == BT_ASSUMED)
    {
      if (DECL_LANG_SPECIFIC (desc) && GFC_DECL_SAVED_DESCRIPTOR (desc))
	desc = GFC_DECL_SAVED_DESCRIPTOR (desc);
      if (POINTER_TYPE_P (TREE_TYPE (desc)))
	desc = build_fold_indirect_ref_loc (input_location, desc);
      tmp = gfc_conv_descriptor_span_get (desc);
    }
  else if (TREE_CODE (desc) == COMPONENT_REF
	   && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc))
	   && GFC_CLASS_TYPE_P (TREE_TYPE (TREE_OPERAND (desc, 0))))
    {
      /* The descriptor is a class _data field and so use the vtable
	 size for the receiving span field.  */
      tmp = gfc_get_vptr_from_expr (desc);
      tmp = gfc_vptr_size_get (tmp);
    }
  else if (expr && expr->expr_type == EXPR_VARIABLE
	   && expr->symtree->n.sym->ts.type == BT_CLASS
	   && expr->ref->type == REF_COMPONENT
	   && expr->ref->next->type == REF_ARRAY
	   && expr->ref->next->next == NULL
	   && CLASS_DATA (expr->symtree->n.sym)->attr.dimension)
    {
      /* Dummys come in sometimes with the descriptor detached from
	 the class field or declaration.  */
      tmp = gfc_class_vptr_get (expr->symtree->n.sym->backend_decl);
      tmp = gfc_vptr_size_get (tmp);
    }
  else
    {
      /* If none of the fancy stuff works, the span is the element
	 size of the array. Attempt to deal with unbounded character
	 types if possible. Otherwise, return NULL_TREE.  */
      tmp = gfc_get_element_type (TREE_TYPE (desc));
      if (tmp && TREE_CODE (tmp) == ARRAY_TYPE && TYPE_STRING_FLAG (tmp))
	{
	  gcc_assert (expr->ts.type == BT_CHARACTER);

	  tmp = gfc_get_character_len_in_bytes (tmp);

	  if (tmp == NULL_TREE || integer_zerop (tmp))
	    {
	      tree bs;

	      tmp = gfc_get_expr_charlen (expr);
	      tmp = fold_convert (gfc_array_index_type, tmp);
	      bs = build_int_cst (gfc_array_index_type, expr->ts.kind);
	      tmp = fold_build2_loc (input_location, MULT_EXPR,
				     gfc_array_index_type, tmp, bs);
	    }

	  tmp = (tmp && !integer_zerop (tmp))
	    ? (fold_convert (gfc_array_index_type, tmp)) : (NULL_TREE);
	}
      else
	tmp = fold_convert (gfc_array_index_type,
			    size_in_bytes (tmp));
    }
  return tmp;
}


/* Generate an initializer for a static pointer or allocatable array.  */

void
gfc_trans_static_array_pointer (gfc_symbol * sym)
{
  tree type;

  gcc_assert (TREE_STATIC (sym->backend_decl));
  /* Just zero the data member.  */
  type = TREE_TYPE (sym->backend_decl);
  DECL_INITIAL (sym->backend_decl) = gfc_build_null_descriptor (type);
}


/* If the bounds of SE's loop have not yet been set, see if they can be
   determined from array spec AS, which is the array spec of a called
   function.  MAPPING maps the callee's dummy arguments to the values
   that the caller is passing.  Add any initialization and finalization
   code to SE.  */

void
gfc_set_loop_bounds_from_array_spec (gfc_interface_mapping * mapping,
				     gfc_se * se, gfc_array_spec * as)
{
  int n, dim, total_dim;
  gfc_se tmpse;
  gfc_ss *ss;
  tree lower;
  tree upper;
  tree tmp;

  total_dim = 0;

  if (!as || as->type != AS_EXPLICIT)
    return;

  for (ss = se->ss; ss; ss = ss->parent)
    {
      total_dim += ss->loop->dimen;
      for (n = 0; n < ss->loop->dimen; n++)
	{
	  /* The bound is known, nothing to do.  */
	  if (ss->loop->to[n] != NULL_TREE)
	    continue;

	  dim = ss->dim[n];
	  gcc_assert (dim < as->rank);
	  gcc_assert (ss->loop->dimen <= as->rank);

	  /* Evaluate the lower bound.  */
	  gfc_init_se (&tmpse, NULL);
	  gfc_apply_interface_mapping (mapping, &tmpse, as->lower[dim]);
	  gfc_add_block_to_block (&se->pre, &tmpse.pre);
	  gfc_add_block_to_block (&se->post, &tmpse.post);
	  lower = fold_convert (gfc_array_index_type, tmpse.expr);

	  /* ...and the upper bound.  */
	  gfc_init_se (&tmpse, NULL);
	  gfc_apply_interface_mapping (mapping, &tmpse, as->upper[dim]);
	  gfc_add_block_to_block (&se->pre, &tmpse.pre);
	  gfc_add_block_to_block (&se->post, &tmpse.post);
	  upper = fold_convert (gfc_array_index_type, tmpse.expr);

	  /* Set the upper bound of the loop to UPPER - LOWER.  */
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type, upper, lower);
	  tmp = gfc_evaluate_now (tmp, &se->pre);
	  ss->loop->to[n] = tmp;
	}
    }

  gcc_assert (total_dim == as->rank);
}


/* Generate code to allocate an array temporary, or create a variable to
   hold the data.  If size is NULL, zero the descriptor so that the
   callee will allocate the array.  If DEALLOC is true, also generate code to
   free the array afterwards.

   If INITIAL is not NULL, it is packed using internal_pack and the result used
   as data instead of allocating a fresh, unitialized area of memory.

   Initialization code is added to PRE and finalization code to POST.
   DYNAMIC is true if the caller may want to extend the array later
   using realloc.  This prevents us from putting the array on the stack.  */

static void
gfc_trans_allocate_array_storage (stmtblock_t * pre, stmtblock_t * post,
				  gfc_array_info * info, tree size, tree nelem,
				  tree initial, bool dynamic, bool dealloc)
{
  tree tmp;
  tree desc;
  bool onstack;

  desc = info->descriptor;
  info->offset = gfc_index_zero_node;
  if (size == NULL_TREE || (dynamic && integer_zerop (size)))
    {
      /* A callee allocated array.  */
      gfc_conv_descriptor_data_set (pre, desc, null_pointer_node);
      onstack = false;
    }
  else
    {
      /* Allocate the temporary.  */
      onstack = !dynamic && initial == NULL_TREE
			 && (flag_stack_arrays
			     || gfc_can_put_var_on_stack (size));

      if (onstack)
	{
	  /* Make a temporary variable to hold the data.  */
	  tmp = fold_build2_loc (input_location, MINUS_EXPR, TREE_TYPE (nelem),
				 nelem, gfc_index_one_node);
	  tmp = gfc_evaluate_now (tmp, pre);
	  tmp = build_range_type (gfc_array_index_type, gfc_index_zero_node,
				  tmp);
	  tmp = build_array_type (gfc_get_element_type (TREE_TYPE (desc)),
				  tmp);
	  tmp = gfc_create_var (tmp, "A");
	  /* If we're here only because of -fstack-arrays we have to
	     emit a DECL_EXPR to make the gimplifier emit alloca calls.  */
	  if (!gfc_can_put_var_on_stack (size))
	    gfc_add_expr_to_block (pre,
				   fold_build1_loc (input_location,
						    DECL_EXPR, TREE_TYPE (tmp),
						    tmp));
	  tmp = gfc_build_addr_expr (NULL_TREE, tmp);
	  gfc_conv_descriptor_data_set (pre, desc, tmp);
	}
      else
	{
	  /* Allocate memory to hold the data or call internal_pack.  */
	  if (initial == NULL_TREE)
	    {
	      tmp = gfc_call_malloc (pre, NULL, size);
	      tmp = gfc_evaluate_now (tmp, pre);
	    }
	  else
	    {
	      tree packed;
	      tree source_data;
	      tree was_packed;
	      stmtblock_t do_copying;

	      tmp = TREE_TYPE (initial); /* Pointer to descriptor.  */
	      gcc_assert (TREE_CODE (tmp) == POINTER_TYPE);
	      tmp = TREE_TYPE (tmp); /* The descriptor itself.  */
	      tmp = gfc_get_element_type (tmp);
	      packed = gfc_create_var (build_pointer_type (tmp), "data");

	      tmp = build_call_expr_loc (input_location,
				     gfor_fndecl_in_pack, 1, initial);
	      tmp = fold_convert (TREE_TYPE (packed), tmp);
	      gfc_add_modify (pre, packed, tmp);

	      tmp = build_fold_indirect_ref_loc (input_location,
					     initial);
	      source_data = gfc_conv_descriptor_data_get (tmp);

	      /* internal_pack may return source->data without any allocation
		 or copying if it is already packed.  If that's the case, we
		 need to allocate and copy manually.  */

	      gfc_start_block (&do_copying);
	      tmp = gfc_call_malloc (&do_copying, NULL, size);
	      tmp = fold_convert (TREE_TYPE (packed), tmp);
	      gfc_add_modify (&do_copying, packed, tmp);
	      tmp = gfc_build_memcpy_call (packed, source_data, size);
	      gfc_add_expr_to_block (&do_copying, tmp);

	      was_packed = fold_build2_loc (input_location, EQ_EXPR,
					    logical_type_node, packed,
					    source_data);
	      tmp = gfc_finish_block (&do_copying);
	      tmp = build3_v (COND_EXPR, was_packed, tmp,
			      build_empty_stmt (input_location));
	      gfc_add_expr_to_block (pre, tmp);

	      tmp = fold_convert (pvoid_type_node, packed);
	    }

	  gfc_conv_descriptor_data_set (pre, desc, tmp);
	}
    }
  info->data = gfc_conv_descriptor_data_get (desc);

  /* The offset is zero because we create temporaries with a zero
     lower bound.  */
  gfc_conv_descriptor_offset_set (pre, desc, gfc_index_zero_node);

  if (dealloc && !onstack)
    {
      /* Free the temporary.  */
      tmp = gfc_conv_descriptor_data_get (desc);
      tmp = gfc_call_free (tmp);
      gfc_add_expr_to_block (post, tmp);
    }
}


/* Get the scalarizer array dimension corresponding to actual array dimension
   given by ARRAY_DIM.

   For example, if SS represents the array ref a(1,:,:,1), it is a
   bidimensional scalarizer array, and the result would be 0 for ARRAY_DIM=1,
   and 1 for ARRAY_DIM=2.
   If SS represents transpose(a(:,1,1,:)), it is again a bidimensional
   scalarizer array, and the result would be 1 for ARRAY_DIM=0 and 0 for
   ARRAY_DIM=3.
   If SS represents sum(a(:,:,:,1), dim=1), it is a 2+1-dimensional scalarizer
   array.  If called on the inner ss, the result would be respectively 0,1,2 for
   ARRAY_DIM=0,1,2.  If called on the outer ss, the result would be 0,1
   for ARRAY_DIM=1,2.  */

static int
get_scalarizer_dim_for_array_dim (gfc_ss *ss, int array_dim)
{
  int array_ref_dim;
  int n;

  array_ref_dim = 0;

  for (; ss; ss = ss->parent)
    for (n = 0; n < ss->dimen; n++)
      if (ss->dim[n] < array_dim)
	array_ref_dim++;

  return array_ref_dim;
}


static gfc_ss *
innermost_ss (gfc_ss *ss)
{
  while (ss->nested_ss != NULL)
    ss = ss->nested_ss;

  return ss;
}



/* Get the array reference dimension corresponding to the given loop dimension.
   It is different from the true array dimension given by the dim array in
   the case of a partial array reference (i.e. a(:,:,1,:) for example)
   It is different from the loop dimension in the case of a transposed array.
   */

static int
get_array_ref_dim_for_loop_dim (gfc_ss *ss, int loop_dim)
{
  return get_scalarizer_dim_for_array_dim (innermost_ss (ss),
					   ss->dim[loop_dim]);
}


/* Use the information in the ss to obtain the required information about
   the type and size of an array temporary, when the lhs in an assignment
   is a class expression.  */

static tree
get_class_info_from_ss (stmtblock_t * pre, gfc_ss *ss, tree *eltype)
{
  gfc_ss *lhs_ss;
  gfc_ss *rhs_ss;
  tree tmp;
  tree tmp2;
  tree vptr;
  tree rhs_class_expr = NULL_TREE;
  tree lhs_class_expr = NULL_TREE;
  bool unlimited_rhs = false;
  bool unlimited_lhs = false;
  bool rhs_function = false;
  gfc_symbol *vtab;

  /* The second element in the loop chain contains the source for the
     temporary; ie. the rhs of the assignment.  */
  rhs_ss = ss->loop->ss->loop_chain;

  if (rhs_ss != gfc_ss_terminator
      && rhs_ss->info
      && rhs_ss->info->expr
      && rhs_ss->info->expr->ts.type == BT_CLASS
      && rhs_ss->info->data.array.descriptor)
    {
      if (rhs_ss->info->expr->expr_type != EXPR_VARIABLE)
	rhs_class_expr
	  = gfc_get_class_from_expr (rhs_ss->info->data.array.descriptor);
      else
	rhs_class_expr = gfc_get_class_from_gfc_expr (rhs_ss->info->expr);
      unlimited_rhs = UNLIMITED_POLY (rhs_ss->info->expr);
      if (rhs_ss->info->expr->expr_type == EXPR_FUNCTION)
	rhs_function = true;
    }

  /* For an assignment the lhs is the next element in the loop chain.
     If we have a class rhs, this had better be a class variable
     expression!  */
  lhs_ss = rhs_ss->loop_chain;
  if (lhs_ss != gfc_ss_terminator
      && lhs_ss->info
      && lhs_ss->info->expr
      && lhs_ss->info->expr->expr_type ==EXPR_VARIABLE
      && lhs_ss->info->expr->ts.type == BT_CLASS)
    {
      tmp = lhs_ss->info->data.array.descriptor;
      unlimited_lhs = UNLIMITED_POLY (rhs_ss->info->expr);
    }
  else
    tmp = NULL_TREE;

  /* Get the lhs class expression.  */
  if (tmp != NULL_TREE && lhs_ss->loop_chain == gfc_ss_terminator)
    lhs_class_expr = gfc_get_class_from_expr (tmp);
  else
    return rhs_class_expr;

  gcc_assert (GFC_CLASS_TYPE_P (TREE_TYPE (lhs_class_expr)));

  /* Set the lhs vptr and, if necessary, the _len field.  */
  if (rhs_class_expr)
    {
      /* Both lhs and rhs are class expressions.  */
      tmp = gfc_class_vptr_get (lhs_class_expr);
      gfc_add_modify (pre, tmp,
		      fold_convert (TREE_TYPE (tmp),
				    gfc_class_vptr_get (rhs_class_expr)));
      if (unlimited_lhs)
	{
	  tmp = gfc_class_len_get (lhs_class_expr);
	  if (unlimited_rhs)
	    tmp2 = gfc_class_len_get (rhs_class_expr);
	  else
	    tmp2 = build_int_cst (TREE_TYPE (tmp), 0);
	  gfc_add_modify (pre, tmp, tmp2);
	}

      if (rhs_function)
	{
	  tmp = gfc_class_data_get (rhs_class_expr);
	  gfc_conv_descriptor_offset_set (pre, tmp, gfc_index_zero_node);
	}
    }
  else
   {
      /* lhs is class and rhs is intrinsic or derived type.  */
      *eltype = TREE_TYPE (rhs_ss->info->data.array.descriptor);
      *eltype = gfc_get_element_type (*eltype);
      vtab = gfc_find_vtab (&rhs_ss->info->expr->ts);
      vptr = vtab->backend_decl;
      if (vptr == NULL_TREE)
	vptr = gfc_get_symbol_decl (vtab);
      vptr = gfc_build_addr_expr (NULL_TREE, vptr);
      tmp = gfc_class_vptr_get (lhs_class_expr);
      gfc_add_modify (pre, tmp,
		      fold_convert (TREE_TYPE (tmp), vptr));

      if (unlimited_lhs)
	{
	  tmp = gfc_class_len_get (lhs_class_expr);
	  if (rhs_ss->info
	      && rhs_ss->info->expr
	      && rhs_ss->info->expr->ts.type == BT_CHARACTER)
	    tmp2 = build_int_cst (TREE_TYPE (tmp),
				  rhs_ss->info->expr->ts.kind);
	  else
	    tmp2 = build_int_cst (TREE_TYPE (tmp), 0);
	  gfc_add_modify (pre, tmp, tmp2);
	}
    }

  return rhs_class_expr;
}



/* Generate code to create and initialize the descriptor for a temporary
   array.  This is used for both temporaries needed by the scalarizer, and
   functions returning arrays.  Adjusts the loop variables to be
   zero-based, and calculates the loop bounds for callee allocated arrays.
   Allocate the array unless it's callee allocated (we have a callee
   allocated array if 'callee_alloc' is true, or if loop->to[n] is
   NULL_TREE for any n).  Also fills in the descriptor, data and offset
   fields of info if known.  Returns the size of the array, or NULL for a
   callee allocated array.

   'eltype' == NULL signals that the temporary should be a class object.
   The 'initial' expression is used to obtain the size of the dynamic
   type; otherwise the allocation and initialization proceeds as for any
   other expression

   PRE, POST, INITIAL, DYNAMIC and DEALLOC are as for
   gfc_trans_allocate_array_storage.  */

tree
gfc_trans_create_temp_array (stmtblock_t * pre, stmtblock_t * post, gfc_ss * ss,
			     tree eltype, tree initial, bool dynamic,
			     bool dealloc, bool callee_alloc, locus * where)
{
  gfc_loopinfo *loop;
  gfc_ss *s;
  gfc_array_info *info;
  tree from[GFC_MAX_DIMENSIONS], to[GFC_MAX_DIMENSIONS];
  tree type;
  tree desc;
  tree tmp;
  tree size;
  tree nelem;
  tree cond;
  tree or_expr;
  tree elemsize;
  tree class_expr = NULL_TREE;
  int n, dim, tmp_dim;
  int total_dim = 0;

  /* This signals a class array for which we need the size of the
     dynamic type.  Generate an eltype and then the class expression.  */
  if (eltype == NULL_TREE && initial)
    {
      gcc_assert (POINTER_TYPE_P (TREE_TYPE (initial)));
      class_expr = build_fold_indirect_ref_loc (input_location, initial);
      /* Obtain the structure (class) expression.  */
      class_expr = gfc_get_class_from_expr (class_expr);
      gcc_assert (class_expr);
    }

  /* Otherwise, some expressions, such as class functions, arising from
     dependency checking in assignments come here with class element type.
     The descriptor can be obtained from the ss->info and then converted
     to the class object.  */
  if (class_expr == NULL_TREE && GFC_CLASS_TYPE_P (eltype))
    class_expr = get_class_info_from_ss (pre, ss, &eltype);

  /* If the dynamic type is not available, use the declared type.  */
  if (eltype && GFC_CLASS_TYPE_P (eltype))
    eltype = gfc_get_element_type (TREE_TYPE (TYPE_FIELDS (eltype)));

  if (class_expr == NULL_TREE)
    elemsize = fold_convert (gfc_array_index_type,
			     TYPE_SIZE_UNIT (eltype));
  else
    {
      /* Unlimited polymorphic entities are initialised with NULL vptr. They
	 can be tested for by checking if the len field is present. If so
	 test the vptr before using the vtable size.  */
      tmp = gfc_class_vptr_get (class_expr);
      tmp = fold_build2_loc (input_location, NE_EXPR,
			     logical_type_node,
			     tmp, build_int_cst (TREE_TYPE (tmp), 0));
      elemsize = fold_build3_loc (input_location, COND_EXPR,
				  gfc_array_index_type,
				  tmp,
				  gfc_class_vtab_size_get (class_expr),
				  gfc_index_zero_node);
      elemsize = gfc_evaluate_now (elemsize, pre);
      elemsize = gfc_resize_class_size_with_len (pre, class_expr, elemsize);
      /* Casting the data as a character of the dynamic length ensures that
	 assignment of elements works when needed.  */
      eltype = gfc_get_character_type_len (1, elemsize);
    }

  memset (from, 0, sizeof (from));
  memset (to, 0, sizeof (to));

  info = &ss->info->data.array;

  gcc_assert (ss->dimen > 0);
  gcc_assert (ss->loop->dimen == ss->dimen);

  if (warn_array_temporaries && where)
    gfc_warning (OPT_Warray_temporaries,
		 "Creating array temporary at %L", where);

  /* Set the lower bound to zero.  */
  for (s = ss; s; s = s->parent)
    {
      loop = s->loop;

      total_dim += loop->dimen;
      for (n = 0; n < loop->dimen; n++)
	{
	  dim = s->dim[n];

	  /* Callee allocated arrays may not have a known bound yet.  */
	  if (loop->to[n])
	    loop->to[n] = gfc_evaluate_now (
			fold_build2_loc (input_location, MINUS_EXPR,
					 gfc_array_index_type,
					 loop->to[n], loop->from[n]),
			pre);
	  loop->from[n] = gfc_index_zero_node;

	  /* We have just changed the loop bounds, we must clear the
	     corresponding specloop, so that delta calculation is not skipped
	     later in gfc_set_delta.  */
	  loop->specloop[n] = NULL;

	  /* We are constructing the temporary's descriptor based on the loop
	     dimensions.  As the dimensions may be accessed in arbitrary order
	     (think of transpose) the size taken from the n'th loop may not map
	     to the n'th dimension of the array.  We need to reconstruct loop
	     infos in the right order before using it to set the descriptor
	     bounds.  */
	  tmp_dim = get_scalarizer_dim_for_array_dim (ss, dim);
	  from[tmp_dim] = loop->from[n];
	  to[tmp_dim] = loop->to[n];

	  info->delta[dim] = gfc_index_zero_node;
	  info->start[dim] = gfc_index_zero_node;
	  info->end[dim] = gfc_index_zero_node;
	  info->stride[dim] = gfc_index_one_node;
	}
    }

  /* Initialize the descriptor.  */
  type =
    gfc_get_array_type_bounds (eltype, total_dim, 0, from, to, 1,
			       GFC_ARRAY_UNKNOWN, true);
  desc = gfc_create_var (type, "atmp");
  GFC_DECL_PACKED_ARRAY (desc) = 1;

  /* Emit a DECL_EXPR for the variable sized array type in
     GFC_TYPE_ARRAY_DATAPTR_TYPE so the gimplification of its type
     sizes works correctly.  */
  tree arraytype = TREE_TYPE (GFC_TYPE_ARRAY_DATAPTR_TYPE (type));
  if (! TYPE_NAME (arraytype))
    TYPE_NAME (arraytype) = build_decl (UNKNOWN_LOCATION, TYPE_DECL,
					NULL_TREE, arraytype);
  gfc_add_expr_to_block (pre, build1 (DECL_EXPR,
				      arraytype, TYPE_NAME (arraytype)));

  if (class_expr != NULL_TREE)
    {
      tree class_data;
      tree dtype;

      /* Create a class temporary.  */
      tmp = gfc_create_var (TREE_TYPE (class_expr), "ctmp");
      gfc_add_modify (pre, tmp, class_expr);

      /* Assign the new descriptor to the _data field. This allows the
	 vptr _copy to be used for scalarized assignment since the class
	 temporary can be found from the descriptor.  */
      class_data = gfc_class_data_get (tmp);
      tmp = fold_build1_loc (input_location, VIEW_CONVERT_EXPR,
			     TREE_TYPE (desc), desc);
      gfc_add_modify (pre, class_data, tmp);

      /* Take the dtype from the class expression.  */
      dtype = gfc_conv_descriptor_dtype (gfc_class_data_get (class_expr));
      tmp = gfc_conv_descriptor_dtype (class_data);
      gfc_add_modify (pre, tmp, dtype);

      /* Point desc to the class _data field.  */
      desc = class_data;
    }
  else
    {
      /* Fill in the array dtype.  */
      tmp = gfc_conv_descriptor_dtype (desc);
      gfc_add_modify (pre, tmp, gfc_get_dtype (TREE_TYPE (desc)));
    }

  info->descriptor = desc;
  size = gfc_index_one_node;

  /*
     Fill in the bounds and stride.  This is a packed array, so:

     size = 1;
     for (n = 0; n < rank; n++)
       {
	 stride[n] = size
	 delta = ubound[n] + 1 - lbound[n];
	 size = size * delta;
       }
     size = size * sizeof(element);
  */

  or_expr = NULL_TREE;

  /* If there is at least one null loop->to[n], it is a callee allocated
     array.  */
  for (n = 0; n < total_dim; n++)
    if (to[n] == NULL_TREE)
      {
	size = NULL_TREE;
	break;
      }

  if (size == NULL_TREE)
    for (s = ss; s; s = s->parent)
      for (n = 0; n < s->loop->dimen; n++)
	{
	  dim = get_scalarizer_dim_for_array_dim (ss, s->dim[n]);

	  /* For a callee allocated array express the loop bounds in terms
	     of the descriptor fields.  */
	  tmp = fold_build2_loc (input_location,
		MINUS_EXPR, gfc_array_index_type,
		gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[dim]),
		gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[dim]));
	  s->loop->to[n] = tmp;
	}
  else
    {
      for (n = 0; n < total_dim; n++)
	{
	  /* Store the stride and bound components in the descriptor.  */
	  gfc_conv_descriptor_stride_set (pre, desc, gfc_rank_cst[n], size);

	  gfc_conv_descriptor_lbound_set (pre, desc, gfc_rank_cst[n],
					  gfc_index_zero_node);

	  gfc_conv_descriptor_ubound_set (pre, desc, gfc_rank_cst[n], to[n]);

	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type,
				 to[n], gfc_index_one_node);

	  /* Check whether the size for this dimension is negative.  */
	  cond = fold_build2_loc (input_location, LE_EXPR, logical_type_node,
				  tmp, gfc_index_zero_node);
	  cond = gfc_evaluate_now (cond, pre);

	  if (n == 0)
	    or_expr = cond;
	  else
	    or_expr = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				       logical_type_node, or_expr, cond);

	  size = fold_build2_loc (input_location, MULT_EXPR,
				  gfc_array_index_type, size, tmp);
	  size = gfc_evaluate_now (size, pre);
	}
    }

  /* Get the size of the array.  */
  if (size && !callee_alloc)
    {
      /* If or_expr is true, then the extent in at least one
	 dimension is zero and the size is set to zero.  */
      size = fold_build3_loc (input_location, COND_EXPR, gfc_array_index_type,
			      or_expr, gfc_index_zero_node, size);

      nelem = size;
      size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			      size, elemsize);
    }
  else
    {
      nelem = size;
      size = NULL_TREE;
    }

  /* Set the span.  */
  tmp = fold_convert (gfc_array_index_type, elemsize);
  gfc_conv_descriptor_span_set (pre, desc, tmp);

  gfc_trans_allocate_array_storage (pre, post, info, size, nelem, initial,
				    dynamic, dealloc);

  while (ss->parent)
    ss = ss->parent;

  if (ss->dimen > ss->loop->temp_dim)
    ss->loop->temp_dim = ss->dimen;

  return size;
}


/* Return the number of iterations in a loop that starts at START,
   ends at END, and has step STEP.  */

static tree
gfc_get_iteration_count (tree start, tree end, tree step)
{
  tree tmp;
  tree type;

  type = TREE_TYPE (step);
  tmp = fold_build2_loc (input_location, MINUS_EXPR, type, end, start);
  tmp = fold_build2_loc (input_location, FLOOR_DIV_EXPR, type, tmp, step);
  tmp = fold_build2_loc (input_location, PLUS_EXPR, type, tmp,
			 build_int_cst (type, 1));
  tmp = fold_build2_loc (input_location, MAX_EXPR, type, tmp,
			 build_int_cst (type, 0));
  return fold_convert (gfc_array_index_type, tmp);
}


/* Extend the data in array DESC by EXTRA elements.  */

static void
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


/* Return true if the bounds of iterator I can only be determined
   at run time.  */

static inline bool
gfc_iterator_has_dynamic_bounds (gfc_iterator * i)
{
  return (i->start->expr_type != EXPR_CONSTANT
	  || i->end->expr_type != EXPR_CONSTANT
	  || i->step->expr_type != EXPR_CONSTANT);
}


/* Split the size of constructor element EXPR into the sum of two terms,
   one of which can be determined at compile time and one of which must
   be calculated at run time.  Set *SIZE to the former and return true
   if the latter might be nonzero.  */

static bool
gfc_get_array_constructor_element_size (mpz_t * size, gfc_expr * expr)
{
  if (expr->expr_type == EXPR_ARRAY)
    return gfc_get_array_constructor_size (size, expr->value.constructor);
  else if (expr->rank > 0)
    {
      /* Calculate everything at run time.  */
      mpz_set_ui (*size, 0);
      return true;
    }
  else
    {
      /* A single element.  */
      mpz_set_ui (*size, 1);
      return false;
    }
}


/* Like gfc_get_array_constructor_element_size, but applied to the whole
   of array constructor C.  */

static bool
gfc_get_array_constructor_size (mpz_t * size, gfc_constructor_base base)
{
  gfc_constructor *c;
  gfc_iterator *i;
  mpz_t val;
  mpz_t len;
  bool dynamic;

  mpz_set_ui (*size, 0);
  mpz_init (len);
  mpz_init (val);

  dynamic = false;
  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next (c))
    {
      i = c->iterator;
      if (i && gfc_iterator_has_dynamic_bounds (i))
	dynamic = true;
      else
	{
	  dynamic |= gfc_get_array_constructor_element_size (&len, c->expr);
	  if (i)
	    {
	      /* Multiply the static part of the element size by the
		 number of iterations.  */
	      mpz_sub (val, i->end->value.integer, i->start->value.integer);
	      mpz_fdiv_q (val, val, i->step->value.integer);
	      mpz_add_ui (val, val, 1);
	      if (mpz_sgn (val) > 0)
		mpz_mul (len, len, val);
	      else
		mpz_set_ui (len, 0);
	    }
	  mpz_add (*size, *size, len);
	}
    }
  mpz_clear (len);
  mpz_clear (val);
  return dynamic;
}


/* Make sure offset is a variable.  */

static void
gfc_put_offset_into_var (stmtblock_t * pblock, tree * poffset,
			 tree * offsetvar)
{
  /* We should have already created the offset variable.  We cannot
     create it here because we may be in an inner scope.  */
  gcc_assert (*offsetvar != NULL_TREE);
  gfc_add_modify (pblock, *offsetvar, *poffset);
  *poffset = *offsetvar;
  TREE_USED (*offsetvar) = 1;
}


/* Variables needed for bounds-checking.  */
static bool first_len;
static tree first_len_val;
static bool typespec_chararray_ctor;

static void
gfc_trans_array_ctor_element (stmtblock_t * pblock, tree desc,
			      tree offset, gfc_se * se, gfc_expr * expr)
{
  tree tmp;

  gfc_conv_expr (se, expr);

  /* Store the value.  */
  tmp = build_fold_indirect_ref_loc (input_location,
				 gfc_conv_descriptor_data_get (desc));
  tmp = gfc_build_array_ref (tmp, offset, NULL);

  if (expr->ts.type == BT_CHARACTER)
    {
      int i = gfc_validate_kind (BT_CHARACTER, expr->ts.kind, false);
      tree esize;

      esize = size_in_bytes (gfc_get_element_type (TREE_TYPE (desc)));
      esize = fold_convert (gfc_charlen_type_node, esize);
      esize = fold_build2_loc (input_location, TRUNC_DIV_EXPR,
			       TREE_TYPE (esize), esize,
			       build_int_cst (TREE_TYPE (esize),
					  gfc_character_kinds[i].bit_size / 8));

      gfc_conv_string_parameter (se);
      if (POINTER_TYPE_P (TREE_TYPE (tmp)))
	{
	  /* The temporary is an array of pointers.  */
	  se->expr = fold_convert (TREE_TYPE (tmp), se->expr);
	  gfc_add_modify (&se->pre, tmp, se->expr);
	}
      else
	{
	  /* The temporary is an array of string values.  */
	  tmp = gfc_build_addr_expr (gfc_get_pchar_type (expr->ts.kind), tmp);
	  /* We know the temporary and the value will be the same length,
	     so can use memcpy.  */
	  gfc_trans_string_copy (&se->pre, esize, tmp, expr->ts.kind,
				 se->string_length, se->expr, expr->ts.kind);
	}
      if ((gfc_option.rtcheck & GFC_RTCHECK_BOUNDS) && !typespec_chararray_ctor)
	{
	  if (first_len)
	    {
	      gfc_add_modify (&se->pre, first_len_val,
			      fold_convert (TREE_TYPE (first_len_val),
					    se->string_length));
	      first_len = false;
	    }
	  else
	    {
	      /* Verify that all constructor elements are of the same
		 length.  */
	      tree rhs = fold_convert (TREE_TYPE (first_len_val),
				       se->string_length);
	      tree cond = fold_build2_loc (input_location, NE_EXPR,
					   logical_type_node, first_len_val,
					   rhs);
	      gfc_trans_runtime_check
		(true, false, cond, &se->pre, &expr->where,
		 "Different CHARACTER lengths (%ld/%ld) in array constructor",
		 fold_convert (long_integer_type_node, first_len_val),
		 fold_convert (long_integer_type_node, se->string_length));
	    }
	}
    }
  else if (GFC_CLASS_TYPE_P (TREE_TYPE (se->expr))
	   && !GFC_CLASS_TYPE_P (gfc_get_element_type (TREE_TYPE (desc))))
    {
      /* Assignment of a CLASS array constructor to a derived type array.  */
      if (expr->expr_type == EXPR_FUNCTION)
	se->expr = gfc_evaluate_now (se->expr, pblock);
      se->expr = gfc_class_data_get (se->expr);
      se->expr = build_fold_indirect_ref_loc (input_location, se->expr);
      se->expr = fold_convert (TREE_TYPE (tmp), se->expr);
      gfc_add_modify (&se->pre, tmp, se->expr);
    }
  else
    {
      /* TODO: Should the frontend already have done this conversion?  */
      se->expr = fold_convert (TREE_TYPE (tmp), se->expr);
      gfc_add_modify (&se->pre, tmp, se->expr);
    }

  gfc_add_block_to_block (pblock, &se->pre);
  gfc_add_block_to_block (pblock, &se->post);
}


/* Add the contents of an array to the constructor.  DYNAMIC is as for
   gfc_trans_array_constructor_value.  */

static void
gfc_trans_array_constructor_subarray (stmtblock_t * pblock,
				      tree type ATTRIBUTE_UNUSED,
				      tree desc, gfc_expr * expr,
				      tree * poffset, tree * offsetvar,
				      bool dynamic)
{
  gfc_se se;
  gfc_ss *ss;
  gfc_loopinfo loop;
  stmtblock_t body;
  tree tmp;
  tree size;
  int n;

  /* We need this to be a variable so we can increment it.  */
  gfc_put_offset_into_var (pblock, poffset, offsetvar);

  gfc_init_se (&se, NULL);

  /* Walk the array expression.  */
  ss = gfc_walk_expr (expr);
  gcc_assert (ss != gfc_ss_terminator);

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, ss);

  /* Initialize the loop.  */
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, &expr->where);

  /* Make sure the constructed array has room for the new data.  */
  if (dynamic)
    {
      /* Set SIZE to the total number of elements in the subarray.  */
      size = gfc_index_one_node;
      for (n = 0; n < loop.dimen; n++)
	{
	  tmp = gfc_get_iteration_count (loop.from[n], loop.to[n],
					 gfc_index_one_node);
	  size = fold_build2_loc (input_location, MULT_EXPR,
				  gfc_array_index_type, size, tmp);
	}

      /* Grow the constructed array by SIZE elements.  */
      gfc_grow_array (&loop.pre, desc, size);
    }

  /* Make the loop body.  */
  gfc_mark_ss_chain_used (ss, 1);
  gfc_start_scalarized_body (&loop, &body);
  gfc_copy_loopinfo_to_se (&se, &loop);
  se.ss = ss;

  gfc_trans_array_ctor_element (&body, desc, *poffset, &se, expr);
  gcc_assert (se.ss == gfc_ss_terminator);

  /* Increment the offset.  */
  tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			 *poffset, gfc_index_one_node);
  gfc_add_modify (&body, *poffset, tmp);

  /* Finish the loop.  */
  gfc_trans_scalarizing_loops (&loop, &body);
  gfc_add_block_to_block (&loop.pre, &loop.post);
  tmp = gfc_finish_block (&loop.pre);
  gfc_add_expr_to_block (pblock, tmp);

  gfc_cleanup_loop (&loop);
}


/* Assign the values to the elements of an array constructor.  DYNAMIC
   is true if descriptor DESC only contains enough data for the static
   size calculated by gfc_get_array_constructor_size.  When true, memory
   for the dynamic parts must be allocated using realloc.  */

static void
gfc_trans_array_constructor_value (stmtblock_t * pblock,
				   stmtblock_t * finalblock,
				   tree type, tree desc,
				   gfc_constructor_base base, tree * poffset,
				   tree * offsetvar, bool dynamic)
{
  tree tmp;
  tree start = NULL_TREE;
  tree end = NULL_TREE;
  tree step = NULL_TREE;
  stmtblock_t body;
  gfc_se se;
  mpz_t size;
  gfc_constructor *c;
  gfc_typespec ts;
  int ctr = 0;

  tree shadow_loopvar = NULL_TREE;
  gfc_saved_var saved_loopvar;

  ts.type = BT_UNKNOWN;
  mpz_init (size);
  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next (c))
    {
      ctr++;
      /* If this is an iterator or an array, the offset must be a variable.  */
      if ((c->iterator || c->expr->rank > 0) && INTEGER_CST_P (*poffset))
	gfc_put_offset_into_var (pblock, poffset, offsetvar);

      /* Shadowing the iterator avoids changing its value and saves us from
	 keeping track of it. Further, it makes sure that there's always a
	 backend-decl for the symbol, even if there wasn't one before,
	 e.g. in the case of an iterator that appears in a specification
	 expression in an interface mapping.  */
      if (c->iterator)
	{
	  gfc_symbol *sym;
	  tree type;

	  /* Evaluate loop bounds before substituting the loop variable
	     in case they depend on it.  Such a case is invalid, but it is
	     not more expensive to do the right thing here.
	     See PR 44354.  */
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_val (&se, c->iterator->start);
	  gfc_add_block_to_block (pblock, &se.pre);
	  start = gfc_evaluate_now (se.expr, pblock);

	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_val (&se, c->iterator->end);
	  gfc_add_block_to_block (pblock, &se.pre);
	  end = gfc_evaluate_now (se.expr, pblock);

	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_val (&se, c->iterator->step);
	  gfc_add_block_to_block (pblock, &se.pre);
	  step = gfc_evaluate_now (se.expr, pblock);

	  sym = c->iterator->var->symtree->n.sym;
	  type = gfc_typenode_for_spec (&sym->ts);

	  shadow_loopvar = gfc_create_var (type, "shadow_loopvar");
	  gfc_shadow_sym (sym, shadow_loopvar, &saved_loopvar);
	}

      gfc_start_block (&body);

      if (c->expr->expr_type == EXPR_ARRAY)
	{
	  /* Array constructors can be nested.  */
	  gfc_trans_array_constructor_value (&body, finalblock, type,
					     desc, c->expr->value.constructor,
					     poffset, offsetvar, dynamic);
	}
      else if (c->expr->rank > 0)
	{
	  gfc_trans_array_constructor_subarray (&body, type, desc, c->expr,
						poffset, offsetvar, dynamic);
	}
      else
	{
	  /* This code really upsets the gimplifier so don't bother for now.  */
	  gfc_constructor *p;
	  HOST_WIDE_INT n;
	  HOST_WIDE_INT size;

	  p = c;
	  n = 0;
	  while (p && !(p->iterator || p->expr->expr_type != EXPR_CONSTANT))
	    {
	      p = gfc_constructor_next (p);
	      n++;
	    }
	  /* Constructor with few constant elements, or element size not
	     known at compile time (e.g. deferred-length character).  */
	  if (n < 4 || !INTEGER_CST_P (TYPE_SIZE_UNIT (type)))
	    {
	      /* Scalar values.  */
	      gfc_init_se (&se, NULL);
	      gfc_trans_array_ctor_element (&body, desc, *poffset,
					    &se, c->expr);

	      *poffset = fold_build2_loc (input_location, PLUS_EXPR,
					  gfc_array_index_type,
					  *poffset, gfc_index_one_node);
	    }
	  else
	    {
	      /* Collect multiple scalar constants into a constructor.  */
	      vec<constructor_elt, va_gc> *v = NULL;
	      tree init;
	      tree bound;
	      tree tmptype;
	      HOST_WIDE_INT idx = 0;

	      p = c;
              /* Count the number of consecutive scalar constants.  */
	      while (p && !(p->iterator
			    || p->expr->expr_type != EXPR_CONSTANT))
		{
		  gfc_init_se (&se, NULL);
		  gfc_conv_constant (&se, p->expr);

		  if (c->expr->ts.type != BT_CHARACTER)
		    se.expr = fold_convert (type, se.expr);
		  /* For constant character array constructors we build
		     an array of pointers.  */
		  else if (POINTER_TYPE_P (type))
		    se.expr = gfc_build_addr_expr
				(gfc_get_pchar_type (p->expr->ts.kind),
				 se.expr);

                  CONSTRUCTOR_APPEND_ELT (v,
                                          build_int_cst (gfc_array_index_type,
                                                         idx++),
                                          se.expr);
		  c = p;
		  p = gfc_constructor_next (p);
		}

	      bound = size_int (n - 1);
              /* Create an array type to hold them.  */
	      tmptype = build_range_type (gfc_array_index_type,
					  gfc_index_zero_node, bound);
	      tmptype = build_array_type (type, tmptype);

	      init = build_constructor (tmptype, v);
	      TREE_CONSTANT (init) = 1;
	      TREE_STATIC (init) = 1;
	      /* Create a static variable to hold the data.  */
	      tmp = gfc_create_var (tmptype, "data");
	      TREE_STATIC (tmp) = 1;
	      TREE_CONSTANT (tmp) = 1;
	      TREE_READONLY (tmp) = 1;
	      DECL_INITIAL (tmp) = init;
	      init = tmp;

	      /* Use BUILTIN_MEMCPY to assign the values.  */
	      tmp = gfc_conv_descriptor_data_get (desc);
	      tmp = build_fold_indirect_ref_loc (input_location,
					     tmp);
	      tmp = gfc_build_array_ref (tmp, *poffset, NULL);
	      tmp = gfc_build_addr_expr (NULL_TREE, tmp);
	      init = gfc_build_addr_expr (NULL_TREE, init);

	      size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));
	      bound = build_int_cst (size_type_node, n * size);
	      tmp = build_call_expr_loc (input_location,
					 builtin_decl_explicit (BUILT_IN_MEMCPY),
					 3, tmp, init, bound);
	      gfc_add_expr_to_block (&body, tmp);

	      *poffset = fold_build2_loc (input_location, PLUS_EXPR,
				      gfc_array_index_type, *poffset,
				      build_int_cst (gfc_array_index_type, n));
	    }
	  if (!INTEGER_CST_P (*poffset))
            {
              gfc_add_modify (&body, *offsetvar, *poffset);
              *poffset = *offsetvar;
            }

	  if (!c->iterator)
	    ts = c->expr->ts;
	}

      /* The frontend should already have done any expansions
	 at compile-time.  */
      if (!c->iterator)
	{
	  /* Pass the code as is.  */
	  tmp = gfc_finish_block (&body);
	  gfc_add_expr_to_block (pblock, tmp);
	}
      else
	{
	  /* Build the implied do-loop.  */
	  stmtblock_t implied_do_block;
	  tree cond;
	  tree exit_label;
	  tree loopbody;
	  tree tmp2;

	  loopbody = gfc_finish_block (&body);

	  /* Create a new block that holds the implied-do loop. A temporary
	     loop-variable is used.  */
	  gfc_start_block(&implied_do_block);

	  /* Initialize the loop.  */
	  gfc_add_modify (&implied_do_block, shadow_loopvar, start);

	  /* If this array expands dynamically, and the number of iterations
	     is not constant, we won't have allocated space for the static
	     part of C->EXPR's size.  Do that now.  */
	  if (dynamic && gfc_iterator_has_dynamic_bounds (c->iterator))
	    {
	      /* Get the number of iterations.  */
	      tmp = gfc_get_iteration_count (shadow_loopvar, end, step);

	      /* Get the static part of C->EXPR's size.  */
	      gfc_get_array_constructor_element_size (&size, c->expr);
	      tmp2 = gfc_conv_mpz_to_tree (size, gfc_index_integer_kind);

	      /* Grow the array by TMP * TMP2 elements.  */
	      tmp = fold_build2_loc (input_location, MULT_EXPR,
				     gfc_array_index_type, tmp, tmp2);
	      gfc_grow_array (&implied_do_block, desc, tmp);
	    }

	  /* Generate the loop body.  */
	  exit_label = gfc_build_label_decl (NULL_TREE);
	  gfc_start_block (&body);

	  /* Generate the exit condition.  Depending on the sign of
	     the step variable we have to generate the correct
	     comparison.  */
	  tmp = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
				 step, build_int_cst (TREE_TYPE (step), 0));
	  cond = fold_build3_loc (input_location, COND_EXPR,
		      logical_type_node, tmp,
		      fold_build2_loc (input_location, GT_EXPR,
				       logical_type_node, shadow_loopvar, end),
		      fold_build2_loc (input_location, LT_EXPR,
				       logical_type_node, shadow_loopvar, end));
	  tmp = build1_v (GOTO_EXPR, exit_label);
	  TREE_USED (exit_label) = 1;
	  tmp = build3_v (COND_EXPR, cond, tmp,
			  build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&body, tmp);

	  /* The main loop body.  */
	  gfc_add_expr_to_block (&body, loopbody);

	  /* Increase loop variable by step.  */
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 TREE_TYPE (shadow_loopvar), shadow_loopvar,
				 step);
	  gfc_add_modify (&body, shadow_loopvar, tmp);

	  /* Finish the loop.  */
	  tmp = gfc_finish_block (&body);
	  tmp = build1_v (LOOP_EXPR, tmp);
	  gfc_add_expr_to_block (&implied_do_block, tmp);

	  /* Add the exit label.  */
	  tmp = build1_v (LABEL_EXPR, exit_label);
	  gfc_add_expr_to_block (&implied_do_block, tmp);

	  /* Finish the implied-do loop.  */
	  tmp = gfc_finish_block(&implied_do_block);
	  gfc_add_expr_to_block(pblock, tmp);

	  gfc_restore_sym (c->iterator->var->symtree->n.sym, &saved_loopvar);
	}
    }

  /* F2008 4.5.6.3 para 5: If an executable construct references a structure
     constructor or array constructor, the entity created by the constructor is
     finalized after execution of the innermost executable construct containing
     the reference. This, in fact, was later deleted by the Combined Techical
     Corrigenda 1 TO 4 for fortran 2008 (f08/0011).

     Transmit finalization of this constructor through 'finalblock'. */
  if ((gfc_option.allow_std & (GFC_STD_F2008 | GFC_STD_F2003))
      && !(gfc_option.allow_std & GFC_STD_GNU)
      && finalblock != NULL
      && gfc_may_be_finalized (ts)
      && ctr > 0 && desc != NULL_TREE
      && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))
    {
      symbol_attribute attr;
      gfc_se fse;
      gfc_warning (0, "The structure constructor at %C has been"
			 " finalized. This feature was removed by f08/0011."
			 " Use -std=f2018 or -std=gnu to eliminate the"
			 " finalization.");
      attr.pointer = attr.allocatable = 0;
      gfc_init_se (&fse, NULL);
      fse.expr = desc;
      gfc_finalize_tree_expr (&fse, ts.u.derived, attr, 1);
      gfc_add_block_to_block (finalblock, &fse.pre);
      gfc_add_block_to_block (finalblock, &fse.finalblock);
      gfc_add_block_to_block (finalblock, &fse.post);
    }

  mpz_clear (size);
}


/* The array constructor code can create a string length with an operand
   in the form of a temporary variable.  This variable will retain its
   context (current_function_decl).  If we store this length tree in a
   gfc_charlen structure which is shared by a variable in another
   context, the resulting gfc_charlen structure with a variable in a
   different context, we could trip the assertion in expand_expr_real_1
   when it sees that a variable has been created in one context and
   referenced in another.

   If this might be the case, we create a new gfc_charlen structure and
   link it into the current namespace.  */

static void
store_backend_decl (gfc_charlen **clp, tree len, bool force_new_cl)
{
  if (force_new_cl)
    {
      gfc_charlen *new_cl = gfc_new_charlen (gfc_current_ns, *clp);
      *clp = new_cl;
    }
  (*clp)->backend_decl = len;
}

/* A catch-all to obtain the string length for anything that is not
   a substring of non-constant length, a constant, array or variable.  */

static void
get_array_ctor_all_strlen (stmtblock_t *block, gfc_expr *e, tree *len)
{
  gfc_se se;

  /* Don't bother if we already know the length is a constant.  */
  if (*len && INTEGER_CST_P (*len))
    return;

  if (!e->ref && e->ts.u.cl && e->ts.u.cl->length
	&& e->ts.u.cl->length->expr_type == EXPR_CONSTANT)
    {
      /* This is easy.  */
      gfc_conv_const_charlen (e->ts.u.cl);
      *len = e->ts.u.cl->backend_decl;
    }
  else
    {
      /* Otherwise, be brutal even if inefficient.  */
      gfc_init_se (&se, NULL);

      /* No function call, in case of side effects.  */
      se.no_function_call = 1;
      if (e->rank == 0)
	gfc_conv_expr (&se, e);
      else
	gfc_conv_expr_descriptor (&se, e);

      /* Fix the value.  */
      *len = gfc_evaluate_now (se.string_length, &se.pre);

      gfc_add_block_to_block (block, &se.pre);
      gfc_add_block_to_block (block, &se.post);

      store_backend_decl (&e->ts.u.cl, *len, true);
    }
}


/* Figure out the string length of a variable reference expression.
   Used by get_array_ctor_strlen.  */

static void
get_array_ctor_var_strlen (stmtblock_t *block, gfc_expr * expr, tree * len)
{
  gfc_ref *ref;
  gfc_typespec *ts;
  mpz_t char_len;
  gfc_se se;

  /* Don't bother if we already know the length is a constant.  */
  if (*len && INTEGER_CST_P (*len))
    return;

  ts = &expr->symtree->n.sym->ts;
  for (ref = expr->ref; ref; ref = ref->next)
    {
      switch (ref->type)
	{
	case REF_ARRAY:
	  /* Array references don't change the string length.  */
	  if (ts->deferred)
	    get_array_ctor_all_strlen (block, expr, len);
	  break;

	case REF_COMPONENT:
	  /* Use the length of the component.  */
	  ts = &ref->u.c.component->ts;
	  break;

	case REF_SUBSTRING:
	  if (ref->u.ss.end == NULL
	      || ref->u.ss.start->expr_type != EXPR_CONSTANT
	      || ref->u.ss.end->expr_type != EXPR_CONSTANT)
	    {
	      /* Note that this might evaluate expr.  */
	      get_array_ctor_all_strlen (block, expr, len);
	      return;
	    }
	  mpz_init_set_ui (char_len, 1);
	  mpz_add (char_len, char_len, ref->u.ss.end->value.integer);
	  mpz_sub (char_len, char_len, ref->u.ss.start->value.integer);
	  *len = gfc_conv_mpz_to_tree_type (char_len, gfc_charlen_type_node);
	  mpz_clear (char_len);
	  return;

	case REF_INQUIRY:
	  break;

	default:
	 gcc_unreachable ();
	}
    }

  /* A last ditch attempt that is sometimes needed for deferred characters.  */
  if (!ts->u.cl->backend_decl)
    {
      gfc_init_se (&se, NULL);
      if (expr->rank)
	gfc_conv_expr_descriptor (&se, expr);
      else
	gfc_conv_expr (&se, expr);
      gcc_assert (se.string_length != NULL_TREE);
      gfc_add_block_to_block (block, &se.pre);
      ts->u.cl->backend_decl = se.string_length;
    }

  *len = ts->u.cl->backend_decl;
}


/* Figure out the string length of a character array constructor.
   If len is NULL, don't calculate the length; this happens for recursive calls
   when a sub-array-constructor is an element but not at the first position,
   so when we're not interested in the length.
   Returns TRUE if all elements are character constants.  */

bool
get_array_ctor_strlen (stmtblock_t *block, gfc_constructor_base base, tree * len)
{
  gfc_constructor *c;
  bool is_const;

  is_const = true;

  if (gfc_constructor_first (base) == NULL)
    {
      if (len)
	*len = build_int_cstu (gfc_charlen_type_node, 0);
      return is_const;
    }

  /* Loop over all constructor elements to find out is_const, but in len we
     want to store the length of the first, not the last, element.  We can
     of course exit the loop as soon as is_const is found to be false.  */
  for (c = gfc_constructor_first (base);
       c && is_const; c = gfc_constructor_next (c))
    {
      switch (c->expr->expr_type)
	{
	case EXPR_CONSTANT:
	  if (len && !(*len && INTEGER_CST_P (*len)))
	    *len = build_int_cstu (gfc_charlen_type_node,
				   c->expr->value.character.length);
	  break;

	case EXPR_ARRAY:
	  if (!get_array_ctor_strlen (block, c->expr->value.constructor, len))
	    is_const = false;
	  break;

	case EXPR_VARIABLE:
	  is_const = false;
	  if (len)
	    get_array_ctor_var_strlen (block, c->expr, len);
	  break;

	default:
	  is_const = false;
	  if (len)
	    get_array_ctor_all_strlen (block, c->expr, len);
	  break;
	}

      /* After the first iteration, we don't want the length modified.  */
      len = NULL;
    }

  return is_const;
}

/* Check whether the array constructor C consists entirely of constant
   elements, and if so returns the number of those elements, otherwise
   return zero.  Note, an empty or NULL array constructor returns zero.  */

unsigned HOST_WIDE_INT
gfc_constant_array_constructor_p (gfc_constructor_base base)
{
  unsigned HOST_WIDE_INT nelem = 0;

  gfc_constructor *c = gfc_constructor_first (base);
  while (c)
    {
      if (c->iterator
	  || c->expr->rank > 0
	  || c->expr->expr_type != EXPR_CONSTANT)
	return 0;
      c = gfc_constructor_next (c);
      nelem++;
    }
  return nelem;
}


/* Given EXPR, the constant array constructor specified by an EXPR_ARRAY,
   and the tree type of it's elements, TYPE, return a static constant
   variable that is compile-time initialized.  */

tree
gfc_build_constant_array_constructor (gfc_expr * expr, tree type)
{
  tree tmptype, init, tmp;
  HOST_WIDE_INT nelem;
  gfc_constructor *c;
  gfc_array_spec as;
  gfc_se se;
  int i;
  vec<constructor_elt, va_gc> *v = NULL;

  /* First traverse the constructor list, converting the constants
     to tree to build an initializer.  */
  nelem = 0;
  c = gfc_constructor_first (expr->value.constructor);
  while (c)
    {
      gfc_init_se (&se, NULL);
      gfc_conv_constant (&se, c->expr);
      if (c->expr->ts.type != BT_CHARACTER)
	se.expr = fold_convert (type, se.expr);
      else if (POINTER_TYPE_P (type))
	se.expr = gfc_build_addr_expr (gfc_get_pchar_type (c->expr->ts.kind),
				       se.expr);
      CONSTRUCTOR_APPEND_ELT (v, build_int_cst (gfc_array_index_type, nelem),
                              se.expr);
      c = gfc_constructor_next (c);
      nelem++;
    }

  /* Next determine the tree type for the array.  We use the gfortran
     front-end's gfc_get_nodesc_array_type in order to create a suitable
     GFC_ARRAY_TYPE_P that may be used by the scalarizer.  */

  memset (&as, 0, sizeof (gfc_array_spec));

  as.rank = expr->rank;
  as.type = AS_EXPLICIT;
  if (!expr->shape)
    {
      as.lower[0] = gfc_get_int_expr (gfc_default_integer_kind, NULL, 0);
      as.upper[0] = gfc_get_int_expr (gfc_default_integer_kind,
				      NULL, nelem - 1);
    }
  else
    for (i = 0; i < expr->rank; i++)
      {
	int tmp = (int) mpz_get_si (expr->shape[i]);
        as.lower[i] = gfc_get_int_expr (gfc_default_integer_kind, NULL, 0);
        as.upper[i] = gfc_get_int_expr (gfc_default_integer_kind,
					NULL, tmp - 1);
      }

  tmptype = gfc_get_nodesc_array_type (type, &as, PACKED_STATIC, true);

  /* as is not needed anymore.  */
  for (i = 0; i < as.rank + as.corank; i++)
    {
      gfc_free_expr (as.lower[i]);
      gfc_free_expr (as.upper[i]);
    }

  init = build_constructor (tmptype, v);

  TREE_CONSTANT (init) = 1;
  TREE_STATIC (init) = 1;

  tmp = build_decl (input_location, VAR_DECL, create_tmp_var_name ("A"),
		    tmptype);
  DECL_ARTIFICIAL (tmp) = 1;
  DECL_IGNORED_P (tmp) = 1;
  TREE_STATIC (tmp) = 1;
  TREE_CONSTANT (tmp) = 1;
  TREE_READONLY (tmp) = 1;
  DECL_INITIAL (tmp) = init;
  pushdecl (tmp);

  return tmp;
}


/* Translate a constant EXPR_ARRAY array constructor for the scalarizer.
   This mostly initializes the scalarizer state info structure with the
   appropriate values to directly use the array created by the function
   gfc_build_constant_array_constructor.  */

static void
trans_constant_array_constructor (gfc_ss * ss, tree type)
{
  gfc_array_info *info;
  tree tmp;
  int i;

  tmp = gfc_build_constant_array_constructor (ss->info->expr, type);

  info = &ss->info->data.array;

  info->descriptor = tmp;
  info->data = gfc_build_addr_expr (NULL_TREE, tmp);
  info->offset = gfc_index_zero_node;

  for (i = 0; i < ss->dimen; i++)
    {
      info->delta[i] = gfc_index_zero_node;
      info->start[i] = gfc_index_zero_node;
      info->end[i] = gfc_index_zero_node;
      info->stride[i] = gfc_index_one_node;
    }
}


static int
get_rank (gfc_loopinfo *loop)
{
  int rank;

  rank = 0;
  for (; loop; loop = loop->parent)
    rank += loop->dimen;

  return rank;
}


/* Helper routine of gfc_trans_array_constructor to determine if the
   bounds of the loop specified by LOOP are constant and simple enough
   to use with trans_constant_array_constructor.  Returns the
   iteration count of the loop if suitable, and NULL_TREE otherwise.  */

static tree
constant_array_constructor_loop_size (gfc_loopinfo * l)
{
  gfc_loopinfo *loop;
  tree size = gfc_index_one_node;
  tree tmp;
  int i, total_dim;

  total_dim = get_rank (l);

  for (loop = l; loop; loop = loop->parent)
    {
      for (i = 0; i < loop->dimen; i++)
	{
	  /* If the bounds aren't constant, return NULL_TREE.  */
	  if (!INTEGER_CST_P (loop->from[i]) || !INTEGER_CST_P (loop->to[i]))
	    return NULL_TREE;
	  if (!integer_zerop (loop->from[i]))
	    {
	      /* Only allow nonzero "from" in one-dimensional arrays.  */
	      if (total_dim != 1)
		return NULL_TREE;
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     gfc_array_index_type,
				     loop->to[i], loop->from[i]);
	    }
	  else
	    tmp = loop->to[i];
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, tmp, gfc_index_one_node);
	  size = fold_build2_loc (input_location, MULT_EXPR,
				  gfc_array_index_type, size, tmp);
	}
    }

  return size;
}


static tree *
get_loop_upper_bound_for_array (gfc_ss *array, int array_dim)
{
  gfc_ss *ss;
  int n;

  gcc_assert (array->nested_ss == NULL);

  for (ss = array; ss; ss = ss->parent)
    for (n = 0; n < ss->loop->dimen; n++)
      if (array_dim == get_array_ref_dim_for_loop_dim (ss, n))
	return &(ss->loop->to[n]);

  gcc_unreachable ();
}


static gfc_loopinfo *
outermost_loop (gfc_loopinfo * loop)
{
  while (loop->parent != NULL)
    loop = loop->parent;

  return loop;
}


/* Array constructors are handled by constructing a temporary, then using that
   within the scalarization loop.  This is not optimal, but seems by far the
   simplest method.  */

static void
trans_array_constructor (gfc_ss * ss, locus * where)
{
  gfc_constructor_base c;
  tree offset;
  tree offsetvar;
  tree desc;
  tree type;
  tree tmp;
  tree *loop_ubound0;
  bool dynamic;
  bool old_first_len, old_typespec_chararray_ctor;
  tree old_first_len_val;
  gfc_loopinfo *loop, *outer_loop;
  gfc_ss_info *ss_info;
  gfc_expr *expr;
  gfc_ss *s;
  tree neg_len;
  char *msg;
  stmtblock_t finalblock;

  /* Save the old values for nested checking.  */
  old_first_len = first_len;
  old_first_len_val = first_len_val;
  old_typespec_chararray_ctor = typespec_chararray_ctor;

  loop = ss->loop;
  outer_loop = outermost_loop (loop);
  ss_info = ss->info;
  expr = ss_info->expr;

  /* Do bounds-checking here and in gfc_trans_array_ctor_element only if no
     typespec was given for the array constructor.  */
  typespec_chararray_ctor = (expr->ts.type == BT_CHARACTER
			     && expr->ts.u.cl
			     && expr->ts.u.cl->length_from_typespec);

  if ((gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
      && expr->ts.type == BT_CHARACTER && !typespec_chararray_ctor)
    {
      first_len_val = gfc_create_var (gfc_charlen_type_node, "len");
      first_len = true;
    }

  gcc_assert (ss->dimen == ss->loop->dimen);

  c = expr->value.constructor;
  if (expr->ts.type == BT_CHARACTER)
    {
      bool const_string;
      bool force_new_cl = false;

      /* get_array_ctor_strlen walks the elements of the constructor, if a
	 typespec was given, we already know the string length and want the one
	 specified there.  */
      if (typespec_chararray_ctor && expr->ts.u.cl->length
	  && expr->ts.u.cl->length->expr_type != EXPR_CONSTANT)
	{
	  gfc_se length_se;

	  const_string = false;
	  gfc_init_se (&length_se, NULL);
	  gfc_conv_expr_type (&length_se, expr->ts.u.cl->length,
			      gfc_charlen_type_node);
	  ss_info->string_length = length_se.expr;

	  /* Check if the character length is negative.  If it is, then
	     set LEN = 0.  */
	  neg_len = fold_build2_loc (input_location, LT_EXPR,
				     logical_type_node, ss_info->string_length,
				     build_zero_cst (TREE_TYPE
						     (ss_info->string_length)));
	  /* Print a warning if bounds checking is enabled.  */
	  if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
	    {
	      msg = xasprintf ("Negative character length treated as LEN = 0");
	      gfc_trans_runtime_check (false, true, neg_len, &length_se.pre,
				       where, msg);
	      free (msg);
	    }

	  ss_info->string_length
	    = fold_build3_loc (input_location, COND_EXPR,
			       gfc_charlen_type_node, neg_len,
			       build_zero_cst
			       (TREE_TYPE (ss_info->string_length)),
			       ss_info->string_length);
	  ss_info->string_length = gfc_evaluate_now (ss_info->string_length,
						     &length_se.pre);
	  gfc_add_block_to_block (&outer_loop->pre, &length_se.pre);
	  gfc_add_block_to_block (&outer_loop->post, &length_se.post);
	}
      else
	{
	  const_string = get_array_ctor_strlen (&outer_loop->pre, c,
						&ss_info->string_length);
	  force_new_cl = true;

	  /* Initialize "len" with string length for bounds checking.  */
	  if ((gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
	      && !typespec_chararray_ctor
	      && ss_info->string_length)
	    {
	      gfc_se length_se;

	      gfc_init_se (&length_se, NULL);
	      gfc_add_modify (&length_se.pre, first_len_val,
			      fold_convert (TREE_TYPE (first_len_val),
					    ss_info->string_length));
	      ss_info->string_length = gfc_evaluate_now (ss_info->string_length,
							 &length_se.pre);
	      gfc_add_block_to_block (&outer_loop->pre, &length_se.pre);
	      gfc_add_block_to_block (&outer_loop->post, &length_se.post);
	    }
	}

      /* Complex character array constructors should have been taken care of
	 and not end up here.  */
      gcc_assert (ss_info->string_length);

      store_backend_decl (&expr->ts.u.cl, ss_info->string_length, force_new_cl);

      type = gfc_get_character_type_len (expr->ts.kind, ss_info->string_length);
      if (const_string)
	type = build_pointer_type (type);
    }
  else
    type = gfc_typenode_for_spec (expr->ts.type == BT_CLASS
				  ? &CLASS_DATA (expr)->ts : &expr->ts);

  /* See if the constructor determines the loop bounds.  */
  dynamic = false;

  loop_ubound0 = get_loop_upper_bound_for_array (ss, 0);

  if (expr->shape && get_rank (loop) > 1 && *loop_ubound0 == NULL_TREE)
    {
      /* We have a multidimensional parameter.  */
      for (s = ss; s; s = s->parent)
	{
	  int n;
	  for (n = 0; n < s->loop->dimen; n++)
	    {
	      s->loop->from[n] = gfc_index_zero_node;
	      s->loop->to[n] = gfc_conv_mpz_to_tree (expr->shape[s->dim[n]],
						     gfc_index_integer_kind);
	      s->loop->to[n] = fold_build2_loc (input_location, MINUS_EXPR,
						gfc_array_index_type,
						s->loop->to[n],
						gfc_index_one_node);
	    }
	}
    }

  if (*loop_ubound0 == NULL_TREE)
    {
      mpz_t size;

      /* We should have a 1-dimensional, zero-based loop.  */
      gcc_assert (loop->parent == NULL && loop->nested == NULL);
      gcc_assert (loop->dimen == 1);
      gcc_assert (integer_zerop (loop->from[0]));

      /* Split the constructor size into a static part and a dynamic part.
	 Allocate the static size up-front and record whether the dynamic
	 size might be nonzero.  */
      mpz_init (size);
      dynamic = gfc_get_array_constructor_size (&size, c);
      mpz_sub_ui (size, size, 1);
      loop->to[0] = gfc_conv_mpz_to_tree (size, gfc_index_integer_kind);
      mpz_clear (size);
    }

  /* Special case constant array constructors.  */
  if (!dynamic)
    {
      unsigned HOST_WIDE_INT nelem = gfc_constant_array_constructor_p (c);
      if (nelem > 0)
	{
	  tree size = constant_array_constructor_loop_size (loop);
	  if (size && compare_tree_int (size, nelem) == 0)
	    {
	      trans_constant_array_constructor (ss, type);
	      goto finish;
	    }
	}
    }

  gfc_trans_create_temp_array (&outer_loop->pre, &outer_loop->post, ss, type,
			       NULL_TREE, dynamic, true, false, where);

  desc = ss_info->data.array.descriptor;
  offset = gfc_index_zero_node;
  offsetvar = gfc_create_var_np (gfc_array_index_type, "offset");
  suppress_warning (offsetvar);
  TREE_USED (offsetvar) = 0;

  gfc_init_block (&finalblock);
  gfc_trans_array_constructor_value (&outer_loop->pre,
				     expr->must_finalize ? &finalblock : NULL,
				     type, desc, c, &offset, &offsetvar,
				     dynamic);

  /* If the array grows dynamically, the upper bound of the loop variable
     is determined by the array's final upper bound.  */
  if (dynamic)
    {
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type,
			     offsetvar, gfc_index_one_node);
      tmp = gfc_evaluate_now (tmp, &outer_loop->pre);
      gfc_conv_descriptor_ubound_set (&loop->pre, desc, gfc_rank_cst[0], tmp);
      if (*loop_ubound0 && VAR_P (*loop_ubound0))
	gfc_add_modify (&outer_loop->pre, *loop_ubound0, tmp);
      else
	*loop_ubound0 = tmp;
    }

  if (TREE_USED (offsetvar))
    pushdecl (offsetvar);
  else
    gcc_assert (INTEGER_CST_P (offset));

#if 0
  /* Disable bound checking for now because it's probably broken.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
    {
      gcc_unreachable ();
    }
#endif

finish:
  /* Restore old values of globals.  */
  first_len = old_first_len;
  first_len_val = old_first_len_val;
  typespec_chararray_ctor = old_typespec_chararray_ctor;

  /* F2008 4.5.6.3 para 5: If an executable construct references a structure
     constructor or array constructor, the entity created by the constructor is
     finalized after execution of the innermost executable construct containing
     the reference.  */
  if ((expr->ts.type == BT_DERIVED || expr->ts.type == BT_CLASS)
       && finalblock.head != NULL_TREE)
    gfc_add_block_to_block (&loop->post, &finalblock);

}


/* INFO describes a GFC_SS_SECTION in loop LOOP, and this function is
   called after evaluating all of INFO's vector dimensions.  Go through
   each such vector dimension and see if we can now fill in any missing
   loop bounds.  */

static void
set_vector_loop_bounds (gfc_ss * ss)
{
  gfc_loopinfo *loop, *outer_loop;
  gfc_array_info *info;
  gfc_se se;
  tree tmp;
  tree desc;
  tree zero;
  int n;
  int dim;

  outer_loop = outermost_loop (ss->loop);

  info = &ss->info->data.array;

  for (; ss; ss = ss->parent)
    {
      loop = ss->loop;

      for (n = 0; n < loop->dimen; n++)
	{
	  dim = ss->dim[n];
	  if (info->ref->u.ar.dimen_type[dim] != DIMEN_VECTOR
	      || loop->to[n] != NULL)
	    continue;

	  /* Loop variable N indexes vector dimension DIM, and we don't
	     yet know the upper bound of loop variable N.  Set it to the
	     difference between the vector's upper and lower bounds.  */
	  gcc_assert (loop->from[n] == gfc_index_zero_node);
	  gcc_assert (info->subscript[dim]
		      && info->subscript[dim]->info->type == GFC_SS_VECTOR);

	  gfc_init_se (&se, NULL);
	  desc = info->subscript[dim]->info->data.array.descriptor;
	  zero = gfc_rank_cst[0];
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type,
			     gfc_conv_descriptor_ubound_get (desc, zero),
			     gfc_conv_descriptor_lbound_get (desc, zero));
	  tmp = gfc_evaluate_now (tmp, &outer_loop->pre);
	  loop->to[n] = tmp;
	}
    }
}


/* Tells whether a scalar argument to an elemental procedure is saved out
   of a scalarization loop as a value or as a reference.  */

bool
gfc_scalar_elemental_arg_saved_as_reference (gfc_ss_info * ss_info)
{
  if (ss_info->type != GFC_SS_REFERENCE)
    return false;

  if (ss_info->data.scalar.needs_temporary)
    return false;

  /* If the actual argument can be absent (in other words, it can
     be a NULL reference), don't try to evaluate it; pass instead
     the reference directly.  */
  if (ss_info->can_be_null_ref)
    return true;

  /* If the expression is of polymorphic type, it's actual size is not known,
     so we avoid copying it anywhere.  */
  if (ss_info->data.scalar.dummy_arg
      && gfc_dummy_arg_get_typespec (*ss_info->data.scalar.dummy_arg).type
	 == BT_CLASS
      && ss_info->expr->ts.type == BT_CLASS)
    return true;

  /* If the expression is a data reference of aggregate type,
     and the data reference is not used on the left hand side,
     avoid a copy by saving a reference to the content.  */
  if (!ss_info->data.scalar.needs_temporary
      && (ss_info->expr->ts.type == BT_DERIVED
	  || ss_info->expr->ts.type == BT_CLASS)
      && gfc_expr_is_variable (ss_info->expr))
    return true;

  /* Otherwise the expression is evaluated to a temporary variable before the
     scalarization loop.  */
  return false;
}


/* Add the pre and post chains for all the scalar expressions in a SS chain
   to loop.  This is called after the loop parameters have been calculated,
   but before the actual scalarizing loops.  */

static void
gfc_add_loop_ss_code (gfc_loopinfo * loop, gfc_ss * ss, bool subscript,
		      locus * where)
{
  gfc_loopinfo *nested_loop, *outer_loop;
  gfc_se se;
  gfc_ss_info *ss_info;
  gfc_array_info *info;
  gfc_expr *expr;
  int n;

  /* Don't evaluate the arguments for realloc_lhs_loop_for_fcn_call; otherwise,
     arguments could get evaluated multiple times.  */
  if (ss->is_alloc_lhs)
    return;

  outer_loop = outermost_loop (loop);

  /* TODO: This can generate bad code if there are ordering dependencies,
     e.g., a callee allocated function and an unknown size constructor.  */
  gcc_assert (ss != NULL);

  for (; ss != gfc_ss_terminator; ss = ss->loop_chain)
    {
      gcc_assert (ss);

      /* Cross loop arrays are handled from within the most nested loop.  */
      if (ss->nested_ss != NULL)
	continue;

      ss_info = ss->info;
      expr = ss_info->expr;
      info = &ss_info->data.array;

      switch (ss_info->type)
	{
	case GFC_SS_SCALAR:
	  /* Scalar expression.  Evaluate this now.  This includes elemental
	     dimension indices, but not array section bounds.  */
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, expr);
	  gfc_add_block_to_block (&outer_loop->pre, &se.pre);

	  if (expr->ts.type != BT_CHARACTER
	      && !gfc_is_alloc_class_scalar_function (expr))
	    {
	      /* Move the evaluation of scalar expressions outside the
		 scalarization loop, except for WHERE assignments.  */
	      if (subscript)
		se.expr = convert(gfc_array_index_type, se.expr);
	      if (!ss_info->where)
		se.expr = gfc_evaluate_now (se.expr, &outer_loop->pre);
	      gfc_add_block_to_block (&outer_loop->pre, &se.post);
	    }
	  else
	    gfc_add_block_to_block (&outer_loop->post, &se.post);

	  ss_info->data.scalar.value = se.expr;
	  ss_info->string_length = se.string_length;
	  break;

	case GFC_SS_REFERENCE:
	  /* Scalar argument to elemental procedure.  */
	  gfc_init_se (&se, NULL);
	  if (gfc_scalar_elemental_arg_saved_as_reference (ss_info))
	    gfc_conv_expr_reference (&se, expr);
	  else
	    {
	      /* Evaluate the argument outside the loop and pass
		 a reference to the value.  */
	      gfc_conv_expr (&se, expr);
	    }

	  /* Ensure that a pointer to the string is stored.  */
	  if (expr->ts.type == BT_CHARACTER)
	    gfc_conv_string_parameter (&se);

	  gfc_add_block_to_block (&outer_loop->pre, &se.pre);
	  gfc_add_block_to_block (&outer_loop->post, &se.post);
	  if (gfc_is_class_scalar_expr (expr))
	    /* This is necessary because the dynamic type will always be
	       large than the declared type.  In consequence, assigning
	       the value to a temporary could segfault.
	       OOP-TODO: see if this is generally correct or is the value
	       has to be written to an allocated temporary, whose address
	       is passed via ss_info.  */
	    ss_info->data.scalar.value = se.expr;
	  else
	    ss_info->data.scalar.value = gfc_evaluate_now (se.expr,
							   &outer_loop->pre);

	  ss_info->string_length = se.string_length;
	  break;

	case GFC_SS_SECTION:
	  /* Add the expressions for scalar and vector subscripts.  */
	  for (n = 0; n < GFC_MAX_DIMENSIONS; n++)
	    if (info->subscript[n])
	      gfc_add_loop_ss_code (loop, info->subscript[n], true, where);

	  set_vector_loop_bounds (ss);
	  break;

	case GFC_SS_VECTOR:
	  /* Get the vector's descriptor and store it in SS.  */
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_descriptor (&se, expr);
	  gfc_add_block_to_block (&outer_loop->pre, &se.pre);
	  gfc_add_block_to_block (&outer_loop->post, &se.post);
	  info->descriptor = se.expr;
	  break;

	case GFC_SS_INTRINSIC:
	  gfc_add_intrinsic_ss_code (loop, ss);
	  break;

	case GFC_SS_FUNCTION:
	  /* Array function return value.  We call the function and save its
	     result in a temporary for use inside the loop.  */
	  gfc_init_se (&se, NULL);
	  se.loop = loop;
	  se.ss = ss;
	  if (gfc_is_class_array_function (expr))
	    expr->must_finalize = 1;
	  gfc_conv_expr (&se, expr);
	  gfc_add_block_to_block (&outer_loop->pre, &se.pre);
	  gfc_add_block_to_block (&outer_loop->post, &se.post);
	  gfc_add_block_to_block (&outer_loop->post, &se.finalblock);
	  ss_info->string_length = se.string_length;
	  break;

	case GFC_SS_CONSTRUCTOR:
	  if (expr->ts.type == BT_CHARACTER
	      && ss_info->string_length == NULL
	      && expr->ts.u.cl
	      && expr->ts.u.cl->length
	      && expr->ts.u.cl->length->expr_type == EXPR_CONSTANT)
	    {
	      gfc_init_se (&se, NULL);
	      gfc_conv_expr_type (&se, expr->ts.u.cl->length,
				  gfc_charlen_type_node);
	      ss_info->string_length = se.expr;
	      gfc_add_block_to_block (&outer_loop->pre, &se.pre);
	      gfc_add_block_to_block (&outer_loop->post, &se.post);
	    }
	  trans_array_constructor (ss, where);
	  break;

        case GFC_SS_TEMP:
	case GFC_SS_COMPONENT:
          /* Do nothing.  These are handled elsewhere.  */
          break;

	default:
	  gcc_unreachable ();
	}
    }

  if (!subscript)
    for (nested_loop = loop->nested; nested_loop;
	 nested_loop = nested_loop->next)
      gfc_add_loop_ss_code (nested_loop, nested_loop->ss, subscript, where);
}


/* Translate expressions for the descriptor and data pointer of a SS.  */
/*GCC ARRAYS*/

static void
gfc_conv_ss_descriptor (stmtblock_t * block, gfc_ss * ss, int base)
{
  gfc_se se;
  gfc_ss_info *ss_info;
  gfc_array_info *info;
  tree tmp;

  ss_info = ss->info;
  info = &ss_info->data.array;

  /* Get the descriptor for the array to be scalarized.  */
  gcc_assert (ss_info->expr->expr_type == EXPR_VARIABLE);
  gfc_init_se (&se, NULL);
  se.descriptor_only = 1;
  gfc_conv_expr_lhs (&se, ss_info->expr);
  gfc_add_block_to_block (block, &se.pre);
  info->descriptor = se.expr;
  ss_info->string_length = se.string_length;
  ss_info->class_container = se.class_container;

  if (base)
    {
      if (ss_info->expr->ts.type == BT_CHARACTER && !ss_info->expr->ts.deferred
	  && ss_info->expr->ts.u.cl->length == NULL)
	{
	  /* Emit a DECL_EXPR for the variable sized array type in
	     GFC_TYPE_ARRAY_DATAPTR_TYPE so the gimplification of its type
	     sizes works correctly.  */
	  tree arraytype = TREE_TYPE (
		GFC_TYPE_ARRAY_DATAPTR_TYPE (TREE_TYPE (info->descriptor)));
	  if (! TYPE_NAME (arraytype))
	    TYPE_NAME (arraytype) = build_decl (UNKNOWN_LOCATION, TYPE_DECL,
						NULL_TREE, arraytype);
	  gfc_add_expr_to_block (block, build1 (DECL_EXPR, arraytype,
						TYPE_NAME (arraytype)));
	}
      /* Also the data pointer.  */
      tmp = gfc_conv_array_data (se.expr);
      /* If this is a variable or address or a class array, use it directly.
         Otherwise we must evaluate it now to avoid breaking dependency
	 analysis by pulling the expressions for elemental array indices
	 inside the loop.  */
      if (!(DECL_P (tmp)
	    || (TREE_CODE (tmp) == ADDR_EXPR
		&& DECL_P (TREE_OPERAND (tmp, 0)))
	    || (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (se.expr))
		&& TREE_CODE (se.expr) == COMPONENT_REF
		&& GFC_CLASS_TYPE_P (TREE_TYPE (TREE_OPERAND (se.expr, 0))))))
	tmp = gfc_evaluate_now (tmp, block);
      info->data = tmp;

      tmp = gfc_conv_array_offset (se.expr);
      info->offset = gfc_evaluate_now (tmp, block);

      /* Make absolutely sure that the saved_offset is indeed saved
	 so that the variable is still accessible after the loops
	 are translated.  */
      info->saved_offset = info->offset;
    }
}


/* Initialize a gfc_loopinfo structure.  */

void
gfc_init_loopinfo (gfc_loopinfo * loop)
{
  int n;

  memset (loop, 0, sizeof (gfc_loopinfo));
  gfc_init_block (&loop->pre);
  gfc_init_block (&loop->post);

  /* Initially scalarize in order and default to no loop reversal.  */
  for (n = 0; n < GFC_MAX_DIMENSIONS; n++)
    {
      loop->order[n] = n;
      loop->reverse[n] = GFC_INHIBIT_REVERSE;
    }

  loop->ss = gfc_ss_terminator;
}


/* Copies the loop variable info to a gfc_se structure. Does not copy the SS
   chain.  */

void
gfc_copy_loopinfo_to_se (gfc_se * se, gfc_loopinfo * loop)
{
  se->loop = loop;
}


/* Return an expression for the data pointer of an array.  */

tree
gfc_conv_array_data (tree descriptor)
{
  tree type;

  type = TREE_TYPE (descriptor);
  if (GFC_ARRAY_TYPE_P (type))
    {
      if (TREE_CODE (type) == POINTER_TYPE)
        return descriptor;
      else
        {
          /* Descriptorless arrays.  */
	  return gfc_build_addr_expr (NULL_TREE, descriptor);
        }
    }
  else
    return gfc_conv_descriptor_data_get (descriptor);
}


/* Return an expression for the base offset of an array.  */

tree
gfc_conv_array_offset (tree descriptor)
{
  tree type;

  type = TREE_TYPE (descriptor);
  if (GFC_ARRAY_TYPE_P (type))
    return GFC_TYPE_ARRAY_OFFSET (type);
  else
    return gfc_conv_descriptor_offset_get (descriptor);
}


/* Get an expression for the array stride.  */

tree
gfc_conv_array_stride (tree descriptor, int dim)
{
  tree tmp;
  tree type;

  type = TREE_TYPE (descriptor);

  /* For descriptorless arrays use the array size.  */
  tmp = GFC_TYPE_ARRAY_STRIDE (type, dim);
  if (tmp != NULL_TREE)
    return tmp;

  tmp = gfc_conv_descriptor_stride_get (descriptor, gfc_rank_cst[dim]);
  return tmp;
}


/* Like gfc_conv_array_stride, but for the lower bound.  */

tree
gfc_conv_array_lbound (tree descriptor, int dim)
{
  tree tmp;
  tree type;

  type = TREE_TYPE (descriptor);

  tmp = GFC_TYPE_ARRAY_LBOUND (type, dim);
  if (tmp != NULL_TREE)
    return tmp;

  tmp = gfc_conv_descriptor_lbound_get (descriptor, gfc_rank_cst[dim]);
  return tmp;
}


/* Like gfc_conv_array_stride, but for the upper bound.  */

tree
gfc_conv_array_ubound (tree descriptor, int dim)
{
  tree tmp;
  tree type;

  type = TREE_TYPE (descriptor);

  tmp = GFC_TYPE_ARRAY_UBOUND (type, dim);
  if (tmp != NULL_TREE)
    return tmp;

  /* This should only ever happen when passing an assumed shape array
     as an actual parameter.  The value will never be used.  */
  if (GFC_ARRAY_TYPE_P (TREE_TYPE (descriptor)))
    return gfc_index_zero_node;

  tmp = gfc_conv_descriptor_ubound_get (descriptor, gfc_rank_cst[dim]);
  return tmp;
}


/* Generate abridged name of a part-ref for use in bounds-check message.
   Cases:
   (1) for an ordinary array variable x return "x"
   (2) for z a DT scalar and array component x (at level 1) return "z%%x"
   (3) for z a DT scalar and array component x (at level > 1) or
       for z a DT array and array x (at any number of levels): "z...%%x"
 */

static char *
abridged_ref_name (gfc_expr * expr, gfc_array_ref * ar)
{
  gfc_ref *ref;
  gfc_symbol *sym;
  char *ref_name = NULL;
  const char *comp_name = NULL;
  int len_sym, last_len = 0, level = 0;
  bool sym_is_array;

  gcc_assert (expr->expr_type == EXPR_VARIABLE && expr->ref != NULL);

  sym = expr->symtree->n.sym;
  sym_is_array = (sym->ts.type != BT_CLASS
		  ? sym->as != NULL
		  : IS_CLASS_ARRAY (sym));
  len_sym = strlen (sym->name);

  /* Scan ref chain to get name of the array component (when ar != NULL) or
     array section, determine depth and remember its component name.  */
  for (ref = expr->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT
	  && strcmp (ref->u.c.component->name, "_data") != 0)
	{
	  level++;
	  comp_name = ref->u.c.component->name;
	  continue;
	}

      if (ref->type != REF_ARRAY)
	continue;

      if (ar)
	{
	  if (&ref->u.ar == ar)
	    break;
	}
      else if (ref->u.ar.type == AR_SECTION)
	break;
    }

  if (level > 0)
    last_len = strlen (comp_name);

  /* Provide a buffer sufficiently large to hold "x...%%z".  */
  ref_name = XNEWVEC (char, len_sym + last_len + 6);
  strcpy (ref_name, sym->name);

  if (level == 1 && !sym_is_array)
    {
      strcat (ref_name, "%%");
      strcat (ref_name, comp_name);
    }
  else if (level > 0)
    {
      strcat (ref_name, "...%%");
      strcat (ref_name, comp_name);
    }

  return ref_name;
}


/* Generate code to perform an array index bound check.  */

static tree
trans_array_bound_check (gfc_se * se, gfc_ss *ss, tree index, int n,
			 locus * where, bool check_upper,
			 const char *compname = NULL)
{
  tree fault;
  tree tmp_lo, tmp_up;
  tree descriptor;
  char *msg;
  char *ref_name = NULL;
  const char * name = NULL;
  gfc_expr *expr;

  if (!(gfc_option.rtcheck & GFC_RTCHECK_BOUNDS))
    return index;

  descriptor = ss->info->data.array.descriptor;

  index = gfc_evaluate_now (index, &se->pre);

  /* We find a name for the error message.  */
  name = ss->info->expr->symtree->n.sym->name;
  gcc_assert (name != NULL);

  /* When we have a component ref, get name of the array section.
     Note that there can only be one part ref.  */
  expr = ss->info->expr;
  if (expr->ref && !compname)
    name = ref_name = abridged_ref_name (expr, NULL);

  if (VAR_P (descriptor))
    name = IDENTIFIER_POINTER (DECL_NAME (descriptor));

  /* Use given (array component) name.  */
  if (compname)
    name = compname;

  /* If upper bound is present, include both bounds in the error message.  */
  if (check_upper)
    {
      tmp_lo = gfc_conv_array_lbound (descriptor, n);
      tmp_up = gfc_conv_array_ubound (descriptor, n);

      if (name)
	msg = xasprintf ("Index '%%ld' of dimension %d of array '%s' "
			 "outside of expected range (%%ld:%%ld)", n+1, name);
      else
	msg = xasprintf ("Index '%%ld' of dimension %d "
			 "outside of expected range (%%ld:%%ld)", n+1);

      fault = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
			       index, tmp_lo);
      gfc_trans_runtime_check (true, false, fault, &se->pre, where, msg,
			       fold_convert (long_integer_type_node, index),
			       fold_convert (long_integer_type_node, tmp_lo),
			       fold_convert (long_integer_type_node, tmp_up));
      fault = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			       index, tmp_up);
      gfc_trans_runtime_check (true, false, fault, &se->pre, where, msg,
			       fold_convert (long_integer_type_node, index),
			       fold_convert (long_integer_type_node, tmp_lo),
			       fold_convert (long_integer_type_node, tmp_up));
      free (msg);
    }
  else
    {
      tmp_lo = gfc_conv_array_lbound (descriptor, n);

      if (name)
	msg = xasprintf ("Index '%%ld' of dimension %d of array '%s' "
			 "below lower bound of %%ld", n+1, name);
      else
	msg = xasprintf ("Index '%%ld' of dimension %d "
			 "below lower bound of %%ld", n+1);

      fault = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
			       index, tmp_lo);
      gfc_trans_runtime_check (true, false, fault, &se->pre, where, msg,
			       fold_convert (long_integer_type_node, index),
			       fold_convert (long_integer_type_node, tmp_lo));
      free (msg);
    }

  free (ref_name);
  return index;
}


/* Generate code for bounds checking for elemental dimensions.  */

static void
array_bound_check_elemental (gfc_se * se, gfc_ss * ss, gfc_expr * expr)
{
  gfc_array_ref *ar;
  gfc_ref *ref;
  char *var_name = NULL;
  int dim;

  if (expr->expr_type == EXPR_VARIABLE)
    {
      for (ref = expr->ref; ref; ref = ref->next)
	{
	  if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION)
	    {
	      ar = &ref->u.ar;
	      var_name = abridged_ref_name (expr, ar);
	      for (dim = 0; dim < ar->dimen; dim++)
		{
		  if (ar->dimen_type[dim] == DIMEN_ELEMENT)
		    {
		      gfc_se indexse;
		      gfc_init_se (&indexse, NULL);
		      gfc_conv_expr_type (&indexse, ar->start[dim],
					  gfc_array_index_type);
		      trans_array_bound_check (se, ss, indexse.expr, dim,
					       &ar->where,
					       ar->as->type != AS_ASSUMED_SIZE
					       || dim < ar->dimen - 1,
					       var_name);
		    }
		}
	      free (var_name);
	    }
	}
    }
}


/* Return the offset for an index.  Performs bound checking for elemental
   dimensions.  Single element references are processed separately.
   DIM is the array dimension, I is the loop dimension.  */

static tree
conv_array_index_offset (gfc_se * se, gfc_ss * ss, int dim, int i,
			 gfc_array_ref * ar, tree stride)
{
  gfc_array_info *info;
  tree index;
  tree desc;
  tree data;

  info = &ss->info->data.array;

  /* Get the index into the array for this dimension.  */
  if (ar)
    {
      gcc_assert (ar->type != AR_ELEMENT);
      switch (ar->dimen_type[dim])
	{
	case DIMEN_THIS_IMAGE:
	  gcc_unreachable ();
	  break;
	case DIMEN_ELEMENT:
	  /* Elemental dimension.  */
	  gcc_assert (info->subscript[dim]
		      && info->subscript[dim]->info->type == GFC_SS_SCALAR);
	  /* We've already translated this value outside the loop.  */
	  index = info->subscript[dim]->info->data.scalar.value;

	  index = trans_array_bound_check (se, ss, index, dim, &ar->where,
					   ar->as->type != AS_ASSUMED_SIZE
					   || dim < ar->dimen - 1);
	  break;

	case DIMEN_VECTOR:
	  gcc_assert (info && se->loop);
	  gcc_assert (info->subscript[dim]
		      && info->subscript[dim]->info->type == GFC_SS_VECTOR);
	  desc = info->subscript[dim]->info->data.array.descriptor;

	  /* Get a zero-based index into the vector.  */
	  index = fold_build2_loc (input_location, MINUS_EXPR,
				   gfc_array_index_type,
				   se->loop->loopvar[i], se->loop->from[i]);

	  /* Multiply the index by the stride.  */
	  index = fold_build2_loc (input_location, MULT_EXPR,
				   gfc_array_index_type,
				   index, gfc_conv_array_stride (desc, 0));

	  /* Read the vector to get an index into info->descriptor.  */
	  data = build_fold_indirect_ref_loc (input_location,
					  gfc_conv_array_data (desc));
	  index = gfc_build_array_ref (data, index, NULL);
	  index = gfc_evaluate_now (index, &se->pre);
	  index = fold_convert (gfc_array_index_type, index);

	  /* Do any bounds checking on the final info->descriptor index.  */
	  index = trans_array_bound_check (se, ss, index, dim, &ar->where,
					   ar->as->type != AS_ASSUMED_SIZE
					   || dim < ar->dimen - 1);
	  break;

	case DIMEN_RANGE:
	  /* Scalarized dimension.  */
	  gcc_assert (info && se->loop);

	  /* Multiply the loop variable by the stride and delta.  */
	  index = se->loop->loopvar[i];
	  if (!integer_onep (info->stride[dim]))
	    index = fold_build2_loc (input_location, MULT_EXPR,
				     gfc_array_index_type, index,
				     info->stride[dim]);
	  if (!integer_zerop (info->delta[dim]))
	    index = fold_build2_loc (input_location, PLUS_EXPR,
				     gfc_array_index_type, index,
				     info->delta[dim]);
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      /* Temporary array or derived type component.  */
      gcc_assert (se->loop);
      index = se->loop->loopvar[se->loop->order[i]];

      /* Pointer functions can have stride[0] different from unity.
	 Use the stride returned by the function call and stored in
	 the descriptor for the temporary.  */
      if (se->ss && se->ss->info->type == GFC_SS_FUNCTION
	  && se->ss->info->expr
	  && se->ss->info->expr->symtree
	  && se->ss->info->expr->symtree->n.sym->result
	  && se->ss->info->expr->symtree->n.sym->result->attr.pointer)
	stride = gfc_conv_descriptor_stride_get (info->descriptor,
						 gfc_rank_cst[dim]);

      if (info->delta[dim] && !integer_zerop (info->delta[dim]))
	index = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, index, info->delta[dim]);
    }

  /* Multiply by the stride.  */
  if (stride != NULL && !integer_onep (stride))
    index = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			     index, stride);

  return index;
}


/* Build a scalarized array reference using the vptr 'size'.  */

static bool
build_class_array_ref (gfc_se *se, tree base, tree index)
{
  tree size;
  tree decl = NULL_TREE;
  tree tmp;
  gfc_expr *expr = se->ss->info->expr;
  gfc_expr *class_expr;
  gfc_typespec *ts;
  gfc_symbol *sym;

  tmp = !VAR_P (base) ? gfc_get_class_from_expr (base) : NULL_TREE;

  if (tmp != NULL_TREE)
    decl = tmp;
  else
    {
      /* The base expression does not contain a class component, either
	 because it is a temporary array or array descriptor.  Class
	 array functions are correctly resolved above.  */
      if (!expr
	  || (expr->ts.type != BT_CLASS
	      && !gfc_is_class_array_ref (expr, NULL)))
	return false;

      /* Obtain the expression for the class entity or component that is
	 followed by an array reference, which is not an element, so that
	 the span of the array can be obtained.  */
      class_expr = gfc_find_and_cut_at_last_class_ref (expr, false, &ts);

      if (!ts)
	return false;

      sym = (!class_expr && expr) ? expr->symtree->n.sym : NULL;
      if (sym && sym->attr.function
	  && sym == sym->result
	  && sym->backend_decl == current_function_decl)
	/* The temporary is the data field of the class data component
	   of the current function.  */
	decl = gfc_get_fake_result_decl (sym, 0);
      else if (sym)
	{
	  if (decl == NULL_TREE)
	    decl = expr->symtree->n.sym->backend_decl;
	  /* For class arrays the tree containing the class is stored in
	     GFC_DECL_SAVED_DESCRIPTOR of the sym's backend_decl.
	     For all others it's sym's backend_decl directly.  */
	  if (DECL_LANG_SPECIFIC (decl) && GFC_DECL_SAVED_DESCRIPTOR (decl))
	    decl = GFC_DECL_SAVED_DESCRIPTOR (decl);
	}
      else
	decl = gfc_get_class_from_gfc_expr (class_expr);

      if (POINTER_TYPE_P (TREE_TYPE (decl)))
	decl = build_fold_indirect_ref_loc (input_location, decl);

      if (!GFC_CLASS_TYPE_P (TREE_TYPE (decl)))
	return false;
    }

  se->class_vptr = gfc_evaluate_now (gfc_class_vptr_get (decl), &se->pre);

  size = gfc_class_vtab_size_get (decl);
  /* For unlimited polymorphic entities then _len component needs to be
     multiplied with the size.  */
  size = gfc_resize_class_size_with_len (&se->pre, decl, size);
  size = fold_convert (TREE_TYPE (index), size);

  /* Return the element in the se expression.  */
  se->expr = gfc_build_spanned_array_ref (base, index, size);
  return true;
}


/* Indicates that the tree EXPR is a reference to an array that can’t
   have any negative stride.  */

static bool
non_negative_strides_array_p (tree expr)
{
  if (expr == NULL_TREE)
    return false;

  tree type = TREE_TYPE (expr);
  if (POINTER_TYPE_P (type))
    type = TREE_TYPE (type);

  if (TYPE_LANG_SPECIFIC (type))
    {
      gfc_array_kind array_kind = GFC_TYPE_ARRAY_AKIND (type);

      if (array_kind == GFC_ARRAY_ALLOCATABLE
	  || array_kind == GFC_ARRAY_ASSUMED_SHAPE_CONT)
	return true;
    }

  /* An array with descriptor can have negative strides.
     We try to be conservative and return false by default here
     if we don’t recognize a contiguous array instead of
     returning false if we can identify a non-contiguous one.  */
  if (!GFC_ARRAY_TYPE_P (type))
    return false;

  /* If the array was originally a dummy with a descriptor, strides can be
     negative.  */
  if (DECL_P (expr)
      && DECL_LANG_SPECIFIC (expr)
      && GFC_DECL_SAVED_DESCRIPTOR (expr)
      && GFC_DECL_SAVED_DESCRIPTOR (expr) != expr)
    return non_negative_strides_array_p (GFC_DECL_SAVED_DESCRIPTOR (expr));

  return true;
}


/* Build a scalarized reference to an array.  */

static void
gfc_conv_scalarized_array_ref (gfc_se * se, gfc_array_ref * ar,
			       bool tmp_array = false)
{
  gfc_array_info *info;
  tree decl = NULL_TREE;
  tree index;
  tree base;
  gfc_ss *ss;
  gfc_expr *expr;
  int n;

  ss = se->ss;
  expr = ss->info->expr;
  info = &ss->info->data.array;
  if (ar)
    n = se->loop->order[0];
  else
    n = 0;

  index = conv_array_index_offset (se, ss, ss->dim[n], n, ar, info->stride0);
  /* Add the offset for this dimension to the stored offset for all other
     dimensions.  */
  if (info->offset && !integer_zerop (info->offset))
    index = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			     index, info->offset);

  base = build_fold_indirect_ref_loc (input_location, info->data);

  /* Use the vptr 'size' field to access the element of a class array.  */
  if (build_class_array_ref (se, base, index))
    return;

  if (get_CFI_desc (NULL, expr, &decl, ar))
    decl = build_fold_indirect_ref_loc (input_location, decl);

  /* A pointer array component can be detected from its field decl. Fix
     the descriptor, mark the resulting variable decl and pass it to
     gfc_build_array_ref.  */
  if (is_pointer_array (info->descriptor)
      || (expr && expr->ts.deferred && info->descriptor
	  && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (info->descriptor))))
    {
      if (TREE_CODE (info->descriptor) == COMPONENT_REF)
	decl = info->descriptor;
      else if (INDIRECT_REF_P (info->descriptor))
	decl = TREE_OPERAND (info->descriptor, 0);

      if (decl == NULL_TREE)
	decl = info->descriptor;
    }

  bool non_negative_stride = tmp_array
			     || non_negative_strides_array_p (info->descriptor);
  se->expr = gfc_build_array_ref (base, index, decl,
				  non_negative_stride);
}


/* Translate access of temporary array.  */

void
gfc_conv_tmp_array_ref (gfc_se * se)
{
  se->string_length = se->ss->info->string_length;
  gfc_conv_scalarized_array_ref (se, NULL, true);
  gfc_advance_se_ss_chain (se);
}

/* Add T to the offset pair *OFFSET, *CST_OFFSET.  */

static void
add_to_offset (tree *cst_offset, tree *offset, tree t)
{
  if (TREE_CODE (t) == INTEGER_CST)
    *cst_offset = int_const_binop (PLUS_EXPR, *cst_offset, t);
  else
    {
      if (!integer_zerop (*offset))
	*offset = fold_build2_loc (input_location, PLUS_EXPR,
				   gfc_array_index_type, *offset, t);
      else
	*offset = t;
    }
}


static tree
build_array_ref (tree desc, tree offset, tree decl, tree vptr)
{
  tree tmp;
  tree type;
  tree cdesc;

  /* For class arrays the class declaration is stored in the saved
     descriptor.  */
  if (INDIRECT_REF_P (desc)
      && DECL_LANG_SPECIFIC (TREE_OPERAND (desc, 0))
      && GFC_DECL_SAVED_DESCRIPTOR (TREE_OPERAND (desc, 0)))
    cdesc = gfc_class_data_get (GFC_DECL_SAVED_DESCRIPTOR (
				  TREE_OPERAND (desc, 0)));
  else
    cdesc = desc;

  /* Class container types do not always have the GFC_CLASS_TYPE_P
     but the canonical type does.  */
  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (cdesc))
      && TREE_CODE (cdesc) == COMPONENT_REF)
    {
      type = TREE_TYPE (TREE_OPERAND (cdesc, 0));
      if (TYPE_CANONICAL (type)
	  && GFC_CLASS_TYPE_P (TYPE_CANONICAL (type)))
	vptr = gfc_class_vptr_get (TREE_OPERAND (cdesc, 0));
    }

  tmp = gfc_conv_array_data (desc);
  tmp = build_fold_indirect_ref_loc (input_location, tmp);
  tmp = gfc_build_array_ref (tmp, offset, decl,
			     non_negative_strides_array_p (desc),
			     vptr);
  return tmp;
}


/* Build an array reference.  se->expr already holds the array descriptor.
   This should be either a variable, indirect variable reference or component
   reference.  For arrays which do not have a descriptor, se->expr will be
   the data pointer.
   a(i, j, k) = base[offset + i * stride[0] + j * stride[1] + k * stride[2]]*/

void
gfc_conv_array_ref (gfc_se * se, gfc_array_ref * ar, gfc_expr *expr,
		    locus * where)
{
  int n;
  tree offset, cst_offset;
  tree tmp;
  tree stride;
  tree decl = NULL_TREE;
  gfc_se indexse;
  gfc_se tmpse;
  gfc_symbol * sym = expr->symtree->n.sym;
  char *var_name = NULL;

  if (ar->dimen == 0)
    {
      gcc_assert (ar->codimen || sym->attr.select_rank_temporary
		  || (ar->as && ar->as->corank));

      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (se->expr)))
	se->expr = build_fold_indirect_ref (gfc_conv_array_data (se->expr));
      else
	{
	  if (GFC_ARRAY_TYPE_P (TREE_TYPE (se->expr))
	      && TREE_CODE (TREE_TYPE (se->expr)) == POINTER_TYPE)
	    se->expr = build_fold_indirect_ref_loc (input_location, se->expr);

	  /* Use the actual tree type and not the wrapped coarray.  */
	  if (!se->want_pointer)
	    se->expr = fold_convert (TYPE_MAIN_VARIANT (TREE_TYPE (se->expr)),
				     se->expr);
	}

      return;
    }

  /* Handle scalarized references separately.  */
  if (ar->type != AR_ELEMENT)
    {
      gfc_conv_scalarized_array_ref (se, ar);
      gfc_advance_se_ss_chain (se);
      return;
    }

  if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
    var_name = abridged_ref_name (expr, ar);

  decl = se->expr;
  if (UNLIMITED_POLY(sym)
      && IS_CLASS_ARRAY (sym)
      && sym->attr.dummy
      && ar->as->type != AS_DEFERRED)
    decl = sym->backend_decl;

  cst_offset = offset = gfc_index_zero_node;
  add_to_offset (&cst_offset, &offset, gfc_conv_array_offset (decl));

  /* Calculate the offsets from all the dimensions.  Make sure to associate
     the final offset so that we form a chain of loop invariant summands.  */
  for (n = ar->dimen - 1; n >= 0; n--)
    {
      /* Calculate the index for this dimension.  */
      gfc_init_se (&indexse, se);
      gfc_conv_expr_type (&indexse, ar->start[n], gfc_array_index_type);
      gfc_add_block_to_block (&se->pre, &indexse.pre);

      if ((gfc_option.rtcheck & GFC_RTCHECK_BOUNDS) && ! expr->no_bounds_check)
	{
	  /* Check array bounds.  */
	  tree cond;
	  char *msg;

	  /* Evaluate the indexse.expr only once.  */
	  indexse.expr = save_expr (indexse.expr);

	  /* Lower bound.  */
	  tmp = gfc_conv_array_lbound (decl, n);
	  if (sym->attr.temporary)
	    {
	      gfc_init_se (&tmpse, se);
	      gfc_conv_expr_type (&tmpse, ar->as->lower[n],
				  gfc_array_index_type);
	      gfc_add_block_to_block (&se->pre, &tmpse.pre);
	      tmp = tmpse.expr;
	    }

	  cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				  indexse.expr, tmp);
	  msg = xasprintf ("Index '%%ld' of dimension %d of array '%s' "
			   "below lower bound of %%ld", n+1, var_name);
	  gfc_trans_runtime_check (true, false, cond, &se->pre, where, msg,
				   fold_convert (long_integer_type_node,
						 indexse.expr),
				   fold_convert (long_integer_type_node, tmp));
	  free (msg);

	  /* Upper bound, but not for the last dimension of assumed-size
	     arrays.  */
	  if (n < ar->dimen - 1 || ar->as->type != AS_ASSUMED_SIZE)
	    {
	      tmp = gfc_conv_array_ubound (decl, n);
	      if (sym->attr.temporary)
		{
		  gfc_init_se (&tmpse, se);
		  gfc_conv_expr_type (&tmpse, ar->as->upper[n],
				      gfc_array_index_type);
		  gfc_add_block_to_block (&se->pre, &tmpse.pre);
		  tmp = tmpse.expr;
		}

	      cond = fold_build2_loc (input_location, GT_EXPR,
				      logical_type_node, indexse.expr, tmp);
	      msg = xasprintf ("Index '%%ld' of dimension %d of array '%s' "
			       "above upper bound of %%ld", n+1, var_name);
	      gfc_trans_runtime_check (true, false, cond, &se->pre, where, msg,
				   fold_convert (long_integer_type_node,
						 indexse.expr),
				   fold_convert (long_integer_type_node, tmp));
	      free (msg);
	    }
	}

      /* Multiply the index by the stride.  */
      stride = gfc_conv_array_stride (decl, n);
      tmp = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			     indexse.expr, stride);

      /* And add it to the total.  */
      add_to_offset (&cst_offset, &offset, tmp);
    }

  if (!integer_zerop (cst_offset))
    offset = fold_build2_loc (input_location, PLUS_EXPR,
			      gfc_array_index_type, offset, cst_offset);

  /* A pointer array component can be detected from its field decl. Fix
     the descriptor, mark the resulting variable decl and pass it to
     build_array_ref.  */
  decl = NULL_TREE;
  if (get_CFI_desc (sym, expr, &decl, ar))
    decl = build_fold_indirect_ref_loc (input_location, decl);
  if (!expr->ts.deferred && !sym->attr.codimension
      && is_pointer_array (se->expr))
    {
      if (TREE_CODE (se->expr) == COMPONENT_REF)
	decl = se->expr;
      else if (INDIRECT_REF_P (se->expr))
	decl = TREE_OPERAND (se->expr, 0);
      else
	decl = se->expr;
    }
  else if (expr->ts.deferred
	   || (sym->ts.type == BT_CHARACTER
	       && sym->attr.select_type_temporary))
    {
      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (se->expr)))
	{
	  decl = se->expr;
	  if (INDIRECT_REF_P (decl))
	    decl = TREE_OPERAND (decl, 0);
	}
      else
	decl = sym->backend_decl;
    }
  else if (sym->ts.type == BT_CLASS)
    {
      if (UNLIMITED_POLY (sym))
	{
	  gfc_expr *class_expr = gfc_find_and_cut_at_last_class_ref (expr);
	  gfc_init_se (&tmpse, NULL);
	  gfc_conv_expr (&tmpse, class_expr);
	  if (!se->class_vptr)
	    se->class_vptr = gfc_class_vptr_get (tmpse.expr);
	  gfc_free_expr (class_expr);
	  decl = tmpse.expr;
	}
      else
	decl = NULL_TREE;
    }

  free (var_name);
  se->expr = build_array_ref (se->expr, offset, decl, se->class_vptr);
}


/* Add the offset corresponding to array's ARRAY_DIM dimension and loop's
   LOOP_DIM dimension (if any) to array's offset.  */

static void
add_array_offset (stmtblock_t *pblock, gfc_loopinfo *loop, gfc_ss *ss,
		  gfc_array_ref *ar, int array_dim, int loop_dim)
{
  gfc_se se;
  gfc_array_info *info;
  tree stride, index;

  info = &ss->info->data.array;

  gfc_init_se (&se, NULL);
  se.loop = loop;
  se.expr = info->descriptor;
  stride = gfc_conv_array_stride (info->descriptor, array_dim);
  index = conv_array_index_offset (&se, ss, array_dim, loop_dim, ar, stride);
  gfc_add_block_to_block (pblock, &se.pre);

  info->offset = fold_build2_loc (input_location, PLUS_EXPR,
				  gfc_array_index_type,
				  info->offset, index);
  info->offset = gfc_evaluate_now (info->offset, pblock);
}


/* Generate the code to be executed immediately before entering a
   scalarization loop.  */

static void
gfc_trans_preloop_setup (gfc_loopinfo * loop, int dim, int flag,
			 stmtblock_t * pblock)
{
  tree stride;
  gfc_ss_info *ss_info;
  gfc_array_info *info;
  gfc_ss_type ss_type;
  gfc_ss *ss, *pss;
  gfc_loopinfo *ploop;
  gfc_array_ref *ar;
  int i;

  /* This code will be executed before entering the scalarization loop
     for this dimension.  */
  for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
    {
      ss_info = ss->info;

      if ((ss_info->useflags & flag) == 0)
	continue;

      ss_type = ss_info->type;
      if (ss_type != GFC_SS_SECTION
	  && ss_type != GFC_SS_FUNCTION
	  && ss_type != GFC_SS_CONSTRUCTOR
	  && ss_type != GFC_SS_COMPONENT)
	continue;

      info = &ss_info->data.array;

      gcc_assert (dim < ss->dimen);
      gcc_assert (ss->dimen == loop->dimen);

      if (info->ref)
	ar = &info->ref->u.ar;
      else
	ar = NULL;

      if (dim == loop->dimen - 1 && loop->parent != NULL)
	{
	  /* If we are in the outermost dimension of this loop, the previous
	     dimension shall be in the parent loop.  */
	  gcc_assert (ss->parent != NULL);

	  pss = ss->parent;
	  ploop = loop->parent;

	  /* ss and ss->parent are about the same array.  */
	  gcc_assert (ss_info == pss->info);
	}
      else
	{
	  ploop = loop;
	  pss = ss;
	}

      if (dim == loop->dimen - 1)
	i = 0;
      else
	i = dim + 1;

      /* For the time being, there is no loop reordering.  */
      gcc_assert (i == ploop->order[i]);
      i = ploop->order[i];

      if (dim == loop->dimen - 1 && loop->parent == NULL)
	{
	  stride = gfc_conv_array_stride (info->descriptor,
					  innermost_ss (ss)->dim[i]);

	  /* Calculate the stride of the innermost loop.  Hopefully this will
	     allow the backend optimizers to do their stuff more effectively.
	   */
	  info->stride0 = gfc_evaluate_now (stride, pblock);

	  /* For the outermost loop calculate the offset due to any
	     elemental dimensions.  It will have been initialized with the
	     base offset of the array.  */
	  if (info->ref)
	    {
	      for (i = 0; i < ar->dimen; i++)
		{
		  if (ar->dimen_type[i] != DIMEN_ELEMENT)
		    continue;

		  add_array_offset (pblock, loop, ss, ar, i, /* unused */ -1);
		}
	    }
	}
      else
	/* Add the offset for the previous loop dimension.  */
	add_array_offset (pblock, ploop, ss, ar, pss->dim[i], i);

      /* Remember this offset for the second loop.  */
      if (dim == loop->temp_dim - 1 && loop->parent == NULL)
        info->saved_offset = info->offset;
    }
}


/* Start a scalarized expression.  Creates a scope and declares loop
   variables.  */

void
gfc_start_scalarized_body (gfc_loopinfo * loop, stmtblock_t * pbody)
{
  int dim;
  int n;
  int flags;

  gcc_assert (!loop->array_parameter);

  for (dim = loop->dimen - 1; dim >= 0; dim--)
    {
      n = loop->order[dim];

      gfc_start_block (&loop->code[n]);

      /* Create the loop variable.  */
      loop->loopvar[n] = gfc_create_var (gfc_array_index_type, "S");

      if (dim < loop->temp_dim)
	flags = 3;
      else
	flags = 1;
      /* Calculate values that will be constant within this loop.  */
      gfc_trans_preloop_setup (loop, dim, flags, &loop->code[n]);
    }
  gfc_start_block (pbody);
}


/* Generates the actual loop code for a scalarization loop.  */

static void
gfc_trans_scalarized_loop_end (gfc_loopinfo * loop, int n,
			       stmtblock_t * pbody)
{
  stmtblock_t block;
  tree cond;
  tree tmp;
  tree loopbody;
  tree exit_label;
  tree stmt;
  tree init;
  tree incr;

  if ((ompws_flags & (OMPWS_WORKSHARE_FLAG | OMPWS_SCALARIZER_WS
		      | OMPWS_SCALARIZER_BODY))
      == (OMPWS_WORKSHARE_FLAG | OMPWS_SCALARIZER_WS)
      && n == loop->dimen - 1)
    {
      /* We create an OMP_FOR construct for the outermost scalarized loop.  */
      init = make_tree_vec (1);
      cond = make_tree_vec (1);
      incr = make_tree_vec (1);

      /* Cycle statement is implemented with a goto.  Exit statement must not
	 be present for this loop.  */
      exit_label = gfc_build_label_decl (NULL_TREE);
      TREE_USED (exit_label) = 1;

      /* Label for cycle statements (if needed).  */
      tmp = build1_v (LABEL_EXPR, exit_label);
      gfc_add_expr_to_block (pbody, tmp);

      stmt = make_node (OMP_FOR);

      TREE_TYPE (stmt) = void_type_node;
      OMP_FOR_BODY (stmt) = loopbody = gfc_finish_block (pbody);

      OMP_FOR_CLAUSES (stmt) = build_omp_clause (input_location,
						 OMP_CLAUSE_SCHEDULE);
      OMP_CLAUSE_SCHEDULE_KIND (OMP_FOR_CLAUSES (stmt))
	= OMP_CLAUSE_SCHEDULE_STATIC;
      if (ompws_flags & OMPWS_NOWAIT)
	OMP_CLAUSE_CHAIN (OMP_FOR_CLAUSES (stmt))
	  = build_omp_clause (input_location, OMP_CLAUSE_NOWAIT);

      /* Initialize the loopvar.  */
      TREE_VEC_ELT (init, 0) = build2_v (MODIFY_EXPR, loop->loopvar[n],
					 loop->from[n]);
      OMP_FOR_INIT (stmt) = init;
      /* The exit condition.  */
      TREE_VEC_ELT (cond, 0) = build2_loc (input_location, LE_EXPR,
					   logical_type_node,
					   loop->loopvar[n], loop->to[n]);
      SET_EXPR_LOCATION (TREE_VEC_ELT (cond, 0), input_location);
      OMP_FOR_COND (stmt) = cond;
      /* Increment the loopvar.  */
      tmp = build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			loop->loopvar[n], gfc_index_one_node);
      TREE_VEC_ELT (incr, 0) = fold_build2_loc (input_location, MODIFY_EXPR,
	  void_type_node, loop->loopvar[n], tmp);
      OMP_FOR_INCR (stmt) = incr;

      ompws_flags &= ~OMPWS_CURR_SINGLEUNIT;
      gfc_add_expr_to_block (&loop->code[n], stmt);
    }
  else
    {
      bool reverse_loop = (loop->reverse[n] == GFC_REVERSE_SET)
			     && (loop->temp_ss == NULL);

      loopbody = gfc_finish_block (pbody);

      if (reverse_loop)
	std::swap (loop->from[n], loop->to[n]);

      /* Initialize the loopvar.  */
      if (loop->loopvar[n] != loop->from[n])
	gfc_add_modify (&loop->code[n], loop->loopvar[n], loop->from[n]);

      exit_label = gfc_build_label_decl (NULL_TREE);

      /* Generate the loop body.  */
      gfc_init_block (&block);

      /* The exit condition.  */
      cond = fold_build2_loc (input_location, reverse_loop ? LT_EXPR : GT_EXPR,
			  logical_type_node, loop->loopvar[n], loop->to[n]);
      tmp = build1_v (GOTO_EXPR, exit_label);
      TREE_USED (exit_label) = 1;
      tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);

      /* The main body.  */
      gfc_add_expr_to_block (&block, loopbody);

      /* Increment the loopvar.  */
      tmp = fold_build2_loc (input_location,
			     reverse_loop ? MINUS_EXPR : PLUS_EXPR,
			     gfc_array_index_type, loop->loopvar[n],
			     gfc_index_one_node);

      gfc_add_modify (&block, loop->loopvar[n], tmp);

      /* Build the loop.  */
      tmp = gfc_finish_block (&block);
      tmp = build1_v (LOOP_EXPR, tmp);
      gfc_add_expr_to_block (&loop->code[n], tmp);

      /* Add the exit label.  */
      tmp = build1_v (LABEL_EXPR, exit_label);
      gfc_add_expr_to_block (&loop->code[n], tmp);
    }

}


/* Finishes and generates the loops for a scalarized expression.  */

void
gfc_trans_scalarizing_loops (gfc_loopinfo * loop, stmtblock_t * body)
{
  int dim;
  int n;
  gfc_ss *ss;
  stmtblock_t *pblock;
  tree tmp;

  pblock = body;
  /* Generate the loops.  */
  for (dim = 0; dim < loop->dimen; dim++)
    {
      n = loop->order[dim];
      gfc_trans_scalarized_loop_end (loop, n, pblock);
      loop->loopvar[n] = NULL_TREE;
      pblock = &loop->code[n];
    }

  tmp = gfc_finish_block (pblock);
  gfc_add_expr_to_block (&loop->pre, tmp);

  /* Clear all the used flags.  */
  for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
    if (ss->parent == NULL)
      ss->info->useflags = 0;
}


/* Finish the main body of a scalarized expression, and start the secondary
   copying body.  */

void
gfc_trans_scalarized_loop_boundary (gfc_loopinfo * loop, stmtblock_t * body)
{
  int dim;
  int n;
  stmtblock_t *pblock;
  gfc_ss *ss;

  pblock = body;
  /* We finish as many loops as are used by the temporary.  */
  for (dim = 0; dim < loop->temp_dim - 1; dim++)
    {
      n = loop->order[dim];
      gfc_trans_scalarized_loop_end (loop, n, pblock);
      loop->loopvar[n] = NULL_TREE;
      pblock = &loop->code[n];
    }

  /* We don't want to finish the outermost loop entirely.  */
  n = loop->order[loop->temp_dim - 1];
  gfc_trans_scalarized_loop_end (loop, n, pblock);

  /* Restore the initial offsets.  */
  for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
    {
      gfc_ss_type ss_type;
      gfc_ss_info *ss_info;

      ss_info = ss->info;

      if ((ss_info->useflags & 2) == 0)
	continue;

      ss_type = ss_info->type;
      if (ss_type != GFC_SS_SECTION
	  && ss_type != GFC_SS_FUNCTION
	  && ss_type != GFC_SS_CONSTRUCTOR
	  && ss_type != GFC_SS_COMPONENT)
	continue;

      ss_info->data.array.offset = ss_info->data.array.saved_offset;
    }

  /* Restart all the inner loops we just finished.  */
  for (dim = loop->temp_dim - 2; dim >= 0; dim--)
    {
      n = loop->order[dim];

      gfc_start_block (&loop->code[n]);

      loop->loopvar[n] = gfc_create_var (gfc_array_index_type, "Q");

      gfc_trans_preloop_setup (loop, dim, 2, &loop->code[n]);
    }

  /* Start a block for the secondary copying code.  */
  gfc_start_block (body);
}


/* Precalculate (either lower or upper) bound of an array section.
     BLOCK: Block in which the (pre)calculation code will go.
     BOUNDS[DIM]: Where the bound value will be stored once evaluated.
     VALUES[DIM]: Specified bound (NULL <=> unspecified).
     DESC: Array descriptor from which the bound will be picked if unspecified
       (either lower or upper bound according to LBOUND).  */

static void
evaluate_bound (stmtblock_t *block, tree *bounds, gfc_expr ** values,
		tree desc, int dim, bool lbound, bool deferred)
{
  gfc_se se;
  gfc_expr * input_val = values[dim];
  tree *output = &bounds[dim];


  if (input_val)
    {
      /* Specified section bound.  */
      gfc_init_se (&se, NULL);
      gfc_conv_expr_type (&se, input_val, gfc_array_index_type);
      gfc_add_block_to_block (block, &se.pre);
      *output = se.expr;
    }
  else if (deferred && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))
    {
      /* The gfc_conv_array_lbound () routine returns a constant zero for
	 deferred length arrays, which in the scalarizer wreaks havoc, when
	 copying to a (newly allocated) one-based array.
	 Keep returning the actual result in sync for both bounds.  */
      *output = lbound ? gfc_conv_descriptor_lbound_get (desc,
							 gfc_rank_cst[dim]):
			 gfc_conv_descriptor_ubound_get (desc,
							 gfc_rank_cst[dim]);
    }
  else
    {
      /* No specific bound specified so use the bound of the array.  */
      *output = lbound ? gfc_conv_array_lbound (desc, dim) :
			 gfc_conv_array_ubound (desc, dim);
    }
  *output = gfc_evaluate_now (*output, block);
}


/* Calculate the lower bound of an array section.  */

static void
gfc_conv_section_startstride (stmtblock_t * block, gfc_ss * ss, int dim)
{
  gfc_expr *stride = NULL;
  tree desc;
  gfc_se se;
  gfc_array_info *info;
  gfc_array_ref *ar;

  gcc_assert (ss->info->type == GFC_SS_SECTION);

  info = &ss->info->data.array;
  ar = &info->ref->u.ar;

  if (ar->dimen_type[dim] == DIMEN_VECTOR)
    {
      /* We use a zero-based index to access the vector.  */
      info->start[dim] = gfc_index_zero_node;
      info->end[dim] = NULL;
      info->stride[dim] = gfc_index_one_node;
      return;
    }

  gcc_assert (ar->dimen_type[dim] == DIMEN_RANGE
	      || ar->dimen_type[dim] == DIMEN_THIS_IMAGE);
  desc = info->descriptor;
  stride = ar->stride[dim];


  /* Calculate the start of the range.  For vector subscripts this will
     be the range of the vector.  */
  evaluate_bound (block, info->start, ar->start, desc, dim, true,
		  ar->as->type == AS_DEFERRED);

  /* Similarly calculate the end.  Although this is not used in the
     scalarizer, it is needed when checking bounds and where the end
     is an expression with side-effects.  */
  evaluate_bound (block, info->end, ar->end, desc, dim, false,
		  ar->as->type == AS_DEFERRED);


  /* Calculate the stride.  */
  if (stride == NULL)
    info->stride[dim] = gfc_index_one_node;
  else
    {
      gfc_init_se (&se, NULL);
      gfc_conv_expr_type (&se, stride, gfc_array_index_type);
      gfc_add_block_to_block (block, &se.pre);
      info->stride[dim] = gfc_evaluate_now (se.expr, block);
    }
}


/* Calculates the range start and stride for a SS chain.  Also gets the
   descriptor and data pointer.  The range of vector subscripts is the size
   of the vector.  Array bounds are also checked.  */

void
gfc_conv_ss_startstride (gfc_loopinfo * loop)
{
  int n;
  tree tmp;
  gfc_ss *ss;
  tree desc;

  gfc_loopinfo * const outer_loop = outermost_loop (loop);

  loop->dimen = 0;
  /* Determine the rank of the loop.  */
  for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
    {
      switch (ss->info->type)
	{
	case GFC_SS_SECTION:
	case GFC_SS_CONSTRUCTOR:
	case GFC_SS_FUNCTION:
	case GFC_SS_COMPONENT:
	  loop->dimen = ss->dimen;
	  goto done;

	/* As usual, lbound and ubound are exceptions!.  */
	case GFC_SS_INTRINSIC:
	  switch (ss->info->expr->value.function.isym->id)
	    {
	    case GFC_ISYM_LBOUND:
	    case GFC_ISYM_UBOUND:
	    case GFC_ISYM_LCOBOUND:
	    case GFC_ISYM_UCOBOUND:
	    case GFC_ISYM_SHAPE:
	    case GFC_ISYM_THIS_IMAGE:
	      loop->dimen = ss->dimen;
	      goto done;

	    default:
	      break;
	    }

	default:
	  break;
	}
    }

  /* We should have determined the rank of the expression by now.  If
     not, that's bad news.  */
  gcc_unreachable ();

done:
  /* Loop over all the SS in the chain.  */
  for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
    {
      gfc_ss_info *ss_info;
      gfc_array_info *info;
      gfc_expr *expr;

      ss_info = ss->info;
      expr = ss_info->expr;
      info = &ss_info->data.array;

      if (expr && expr->shape && !info->shape)
	info->shape = expr->shape;

      switch (ss_info->type)
	{
	case GFC_SS_SECTION:
	  /* Get the descriptor for the array.  If it is a cross loops array,
	     we got the descriptor already in the outermost loop.  */
	  if (ss->parent == NULL)
	    gfc_conv_ss_descriptor (&outer_loop->pre, ss,
				    !loop->array_parameter);

	  for (n = 0; n < ss->dimen; n++)
	    gfc_conv_section_startstride (&outer_loop->pre, ss, ss->dim[n]);
	  break;

	case GFC_SS_INTRINSIC:
	  switch (expr->value.function.isym->id)
	    {
	    /* Fall through to supply start and stride.  */
	    case GFC_ISYM_LBOUND:
	    case GFC_ISYM_UBOUND:
	      /* This is the variant without DIM=...  */
	      gcc_assert (expr->value.function.actual->next->expr == NULL);
	      /* Fall through.  */

	    case GFC_ISYM_SHAPE:
	      {
		gfc_expr *arg;

		arg = expr->value.function.actual->expr;
		if (arg->rank == -1)
		  {
		    gfc_se se;
		    tree rank, tmp;

		    /* The rank (hence the return value's shape) is unknown,
		       we have to retrieve it.  */
		    gfc_init_se (&se, NULL);
		    se.descriptor_only = 1;
		    gfc_conv_expr (&se, arg);
		    /* This is a bare variable, so there is no preliminary
		       or cleanup code.  */
		    gcc_assert (se.pre.head == NULL_TREE
				&& se.post.head == NULL_TREE);
		    rank = gfc_conv_descriptor_rank (se.expr);
		    tmp = fold_build2_loc (input_location, MINUS_EXPR,
					   gfc_array_index_type,
					   fold_convert (gfc_array_index_type,
							 rank),
					   gfc_index_one_node);
		    info->end[0] = gfc_evaluate_now (tmp, &outer_loop->pre);
		    info->start[0] = gfc_index_zero_node;
		    info->stride[0] = gfc_index_one_node;
		    continue;
		  }
		  /* Otherwise fall through GFC_SS_FUNCTION.  */
		  gcc_fallthrough ();
	      }
	    case GFC_ISYM_LCOBOUND:
	    case GFC_ISYM_UCOBOUND:
	    case GFC_ISYM_THIS_IMAGE:
	      break;

	    default:
	      continue;
	    }

	  /* FALLTHRU */
	case GFC_SS_CONSTRUCTOR:
	case GFC_SS_FUNCTION:
	  for (n = 0; n < ss->dimen; n++)
	    {
	      int dim = ss->dim[n];

	      info->start[dim]  = gfc_index_zero_node;
	      info->end[dim]    = gfc_index_zero_node;
	      info->stride[dim] = gfc_index_one_node;
	    }
	  break;

	default:
	  break;
	}
    }

  /* The rest is just runtime bounds checking.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
    {
      stmtblock_t block;
      tree lbound, ubound;
      tree end;
      tree size[GFC_MAX_DIMENSIONS];
      tree stride_pos, stride_neg, non_zerosized, tmp2, tmp3;
      gfc_array_info *info;
      char *msg;
      int dim;

      gfc_start_block (&block);

      for (n = 0; n < loop->dimen; n++)
	size[n] = NULL_TREE;

      /* If there is a constructor involved, derive size[] from its shape.  */
      for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
	{
	  gfc_ss_info *ss_info;

	  ss_info = ss->info;
	  info = &ss_info->data.array;

	  if (ss_info->type == GFC_SS_CONSTRUCTOR && info->shape)
	    {
	      for (n = 0; n < loop->dimen; n++)
		{
		  if (size[n] == NULL)
		    {
		      gcc_assert (info->shape[n]);
		      size[n] = gfc_conv_mpz_to_tree (info->shape[n],
						      gfc_index_integer_kind);
		    }
		}
	      break;
	    }
	}

      for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
	{
	  stmtblock_t inner;
	  gfc_ss_info *ss_info;
	  gfc_expr *expr;
	  locus *expr_loc;
	  const char *expr_name;
	  char *ref_name = NULL;

	  ss_info = ss->info;
	  if (ss_info->type != GFC_SS_SECTION)
	    continue;

	  /* Catch allocatable lhs in f2003.  */
	  if (flag_realloc_lhs && ss->no_bounds_check)
	    continue;

	  expr = ss_info->expr;
	  expr_loc = &expr->where;
	  if (expr->ref)
	    expr_name = ref_name = abridged_ref_name (expr, NULL);
	  else
	    expr_name = expr->symtree->name;

	  gfc_start_block (&inner);

	  /* TODO: range checking for mapped dimensions.  */
	  info = &ss_info->data.array;

	  /* This code only checks ranges.  Elemental and vector
	     dimensions are checked later.  */
	  for (n = 0; n < loop->dimen; n++)
	    {
	      bool check_upper;

	      dim = ss->dim[n];
	      if (info->ref->u.ar.dimen_type[dim] != DIMEN_RANGE)
		continue;

	      if (dim == info->ref->u.ar.dimen - 1
		  && info->ref->u.ar.as->type == AS_ASSUMED_SIZE)
		check_upper = false;
	      else
		check_upper = true;

	      /* Zero stride is not allowed.  */
	      tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				     info->stride[dim], gfc_index_zero_node);
	      msg = xasprintf ("Zero stride is not allowed, for dimension %d "
			       "of array '%s'", dim + 1, expr_name);
	      gfc_trans_runtime_check (true, false, tmp, &inner,
				       expr_loc, msg);
	      free (msg);

	      desc = info->descriptor;

	      /* This is the run-time equivalent of resolve.cc's
		 check_dimension().  The logical is more readable there
		 than it is here, with all the trees.  */
	      lbound = gfc_conv_array_lbound (desc, dim);
	      end = info->end[dim];
	      if (check_upper)
		ubound = gfc_conv_array_ubound (desc, dim);
	      else
		ubound = NULL;

	      /* non_zerosized is true when the selected range is not
		 empty.  */
	      stride_pos = fold_build2_loc (input_location, GT_EXPR,
					logical_type_node, info->stride[dim],
					gfc_index_zero_node);
	      tmp = fold_build2_loc (input_location, LE_EXPR, logical_type_node,
				     info->start[dim], end);
	      stride_pos = fold_build2_loc (input_location, TRUTH_AND_EXPR,
					    logical_type_node, stride_pos, tmp);

	      stride_neg = fold_build2_loc (input_location, LT_EXPR,
				     logical_type_node,
				     info->stride[dim], gfc_index_zero_node);
	      tmp = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
				     info->start[dim], end);
	      stride_neg = fold_build2_loc (input_location, TRUTH_AND_EXPR,
					    logical_type_node,
					    stride_neg, tmp);
	      non_zerosized = fold_build2_loc (input_location, TRUTH_OR_EXPR,
					       logical_type_node,
					       stride_pos, stride_neg);

	      /* Check the start of the range against the lower and upper
		 bounds of the array, if the range is not empty.
	         If upper bound is present, include both bounds in the
		 error message.  */
	      if (check_upper)
		{
		  tmp = fold_build2_loc (input_location, LT_EXPR,
					 logical_type_node,
					 info->start[dim], lbound);
		  tmp = fold_build2_loc (input_location, TRUTH_AND_EXPR,
					 logical_type_node,
					 non_zerosized, tmp);
		  tmp2 = fold_build2_loc (input_location, GT_EXPR,
					  logical_type_node,
					  info->start[dim], ubound);
		  tmp2 = fold_build2_loc (input_location, TRUTH_AND_EXPR,
					  logical_type_node,
					  non_zerosized, tmp2);
		  msg = xasprintf ("Index '%%ld' of dimension %d of array '%s' "
				   "outside of expected range (%%ld:%%ld)",
				   dim + 1, expr_name);
		  gfc_trans_runtime_check (true, false, tmp, &inner,
					   expr_loc, msg,
		     fold_convert (long_integer_type_node, info->start[dim]),
		     fold_convert (long_integer_type_node, lbound),
		     fold_convert (long_integer_type_node, ubound));
		  gfc_trans_runtime_check (true, false, tmp2, &inner,
					   expr_loc, msg,
		     fold_convert (long_integer_type_node, info->start[dim]),
		     fold_convert (long_integer_type_node, lbound),
		     fold_convert (long_integer_type_node, ubound));
		  free (msg);
		}
	      else
		{
		  tmp = fold_build2_loc (input_location, LT_EXPR,
					 logical_type_node,
					 info->start[dim], lbound);
		  tmp = fold_build2_loc (input_location, TRUTH_AND_EXPR,
					 logical_type_node, non_zerosized, tmp);
		  msg = xasprintf ("Index '%%ld' of dimension %d of array '%s' "
				   "below lower bound of %%ld",
				   dim + 1, expr_name);
		  gfc_trans_runtime_check (true, false, tmp, &inner,
					   expr_loc, msg,
		     fold_convert (long_integer_type_node, info->start[dim]),
		     fold_convert (long_integer_type_node, lbound));
		  free (msg);
		}

	      /* Compute the last element of the range, which is not
		 necessarily "end" (think 0:5:3, which doesn't contain 5)
		 and check it against both lower and upper bounds.  */

	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     gfc_array_index_type, end,
				     info->start[dim]);
	      tmp = fold_build2_loc (input_location, TRUNC_MOD_EXPR,
				     gfc_array_index_type, tmp,
				     info->stride[dim]);
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     gfc_array_index_type, end, tmp);
	      tmp2 = fold_build2_loc (input_location, LT_EXPR,
				      logical_type_node, tmp, lbound);
	      tmp2 = fold_build2_loc (input_location, TRUTH_AND_EXPR,
				      logical_type_node, non_zerosized, tmp2);
	      if (check_upper)
		{
		  tmp3 = fold_build2_loc (input_location, GT_EXPR,
					  logical_type_node, tmp, ubound);
		  tmp3 = fold_build2_loc (input_location, TRUTH_AND_EXPR,
					  logical_type_node, non_zerosized, tmp3);
		  msg = xasprintf ("Index '%%ld' of dimension %d of array '%s' "
				   "outside of expected range (%%ld:%%ld)",
				   dim + 1, expr_name);
		  gfc_trans_runtime_check (true, false, tmp2, &inner,
					   expr_loc, msg,
		     fold_convert (long_integer_type_node, tmp),
		     fold_convert (long_integer_type_node, ubound),
		     fold_convert (long_integer_type_node, lbound));
		  gfc_trans_runtime_check (true, false, tmp3, &inner,
					   expr_loc, msg,
		     fold_convert (long_integer_type_node, tmp),
		     fold_convert (long_integer_type_node, ubound),
		     fold_convert (long_integer_type_node, lbound));
		  free (msg);
		}
	      else
		{
		  msg = xasprintf ("Index '%%ld' of dimension %d of array '%s' "
				   "below lower bound of %%ld",
				   dim + 1, expr_name);
		  gfc_trans_runtime_check (true, false, tmp2, &inner,
					   expr_loc, msg,
		     fold_convert (long_integer_type_node, tmp),
		     fold_convert (long_integer_type_node, lbound));
		  free (msg);
		}

	      /* Check the section sizes match.  */
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     gfc_array_index_type, end,
				     info->start[dim]);
	      tmp = fold_build2_loc (input_location, FLOOR_DIV_EXPR,
				     gfc_array_index_type, tmp,
				     info->stride[dim]);
	      tmp = fold_build2_loc (input_location, PLUS_EXPR,
				     gfc_array_index_type,
				     gfc_index_one_node, tmp);
	      tmp = fold_build2_loc (input_location, MAX_EXPR,
				     gfc_array_index_type, tmp,
				     build_int_cst (gfc_array_index_type, 0));
	      /* We remember the size of the first section, and check all the
		 others against this.  */
	      if (size[n])
		{
		  tmp3 = fold_build2_loc (input_location, NE_EXPR,
					  logical_type_node, tmp, size[n]);
		  msg = xasprintf ("Array bound mismatch for dimension %d "
				   "of array '%s' (%%ld/%%ld)",
				   dim + 1, expr_name);

		  gfc_trans_runtime_check (true, false, tmp3, &inner,
					   expr_loc, msg,
			fold_convert (long_integer_type_node, tmp),
			fold_convert (long_integer_type_node, size[n]));

		  free (msg);
		}
	      else
		size[n] = gfc_evaluate_now (tmp, &inner);
	    }

	  tmp = gfc_finish_block (&inner);

	  /* For optional arguments, only check bounds if the argument is
	     present.  */
	  if ((expr->symtree->n.sym->attr.optional
	       || expr->symtree->n.sym->attr.not_always_present)
	      && expr->symtree->n.sym->attr.dummy)
	    tmp = build3_v (COND_EXPR,
			    gfc_conv_expr_present (expr->symtree->n.sym),
			    tmp, build_empty_stmt (input_location));

	  gfc_add_expr_to_block (&block, tmp);

	  free (ref_name);
	}

      tmp = gfc_finish_block (&block);
      gfc_add_expr_to_block (&outer_loop->pre, tmp);
    }

  for (loop = loop->nested; loop; loop = loop->next)
    gfc_conv_ss_startstride (loop);
}

/* Return true if both symbols could refer to the same data object.  Does
   not take account of aliasing due to equivalence statements.  */

static bool
symbols_could_alias (gfc_symbol *lsym, gfc_symbol *rsym, bool lsym_pointer,
		     bool lsym_target, bool rsym_pointer, bool rsym_target)
{
  /* Aliasing isn't possible if the symbols have different base types.  */
  if (gfc_compare_types (&lsym->ts, &rsym->ts) == 0)
    return 0;

  /* Pointers can point to other pointers and target objects.  */

  if ((lsym_pointer && (rsym_pointer || rsym_target))
      || (rsym_pointer && (lsym_pointer || lsym_target)))
    return 1;

  /* Special case: Argument association, cf. F90 12.4.1.6, F2003 12.4.1.7
     and F2008 12.5.2.13 items 3b and 4b. The pointer case (a) is already
     checked above.  */
  if (lsym_target && rsym_target
      && ((lsym->attr.dummy && !lsym->attr.contiguous
	   && (!lsym->attr.dimension || lsym->as->type == AS_ASSUMED_SHAPE))
	  || (rsym->attr.dummy && !rsym->attr.contiguous
	      && (!rsym->attr.dimension
		  || rsym->as->type == AS_ASSUMED_SHAPE))))
    return 1;

  return 0;
}


/* Return true if the two SS could be aliased, i.e. both point to the same data
   object.  */
/* TODO: resolve aliases based on frontend expressions.  */

static int
gfc_could_be_alias (gfc_ss * lss, gfc_ss * rss)
{
  gfc_ref *lref;
  gfc_ref *rref;
  gfc_expr *lexpr, *rexpr;
  gfc_symbol *lsym;
  gfc_symbol *rsym;
  bool lsym_pointer, lsym_target, rsym_pointer, rsym_target;

  lexpr = lss->info->expr;
  rexpr = rss->info->expr;

  lsym = lexpr->symtree->n.sym;
  rsym = rexpr->symtree->n.sym;

  lsym_pointer = lsym->attr.pointer;
  lsym_target = lsym->attr.target;
  rsym_pointer = rsym->attr.pointer;
  rsym_target = rsym->attr.target;

  if (symbols_could_alias (lsym, rsym, lsym_pointer, lsym_target,
			   rsym_pointer, rsym_target))
    return 1;

  if (rsym->ts.type != BT_DERIVED && rsym->ts.type != BT_CLASS
      && lsym->ts.type != BT_DERIVED && lsym->ts.type != BT_CLASS)
    return 0;

  /* For derived types we must check all the component types.  We can ignore
     array references as these will have the same base type as the previous
     component ref.  */
  for (lref = lexpr->ref; lref != lss->info->data.array.ref; lref = lref->next)
    {
      if (lref->type != REF_COMPONENT)
	continue;

      lsym_pointer = lsym_pointer || lref->u.c.sym->attr.pointer;
      lsym_target  = lsym_target  || lref->u.c.sym->attr.target;

      if (symbols_could_alias (lref->u.c.sym, rsym, lsym_pointer, lsym_target,
			       rsym_pointer, rsym_target))
	return 1;

      if ((lsym_pointer && (rsym_pointer || rsym_target))
	  || (rsym_pointer && (lsym_pointer || lsym_target)))
	{
	  if (gfc_compare_types (&lref->u.c.component->ts,
				 &rsym->ts))
	    return 1;
	}

      for (rref = rexpr->ref; rref != rss->info->data.array.ref;
	   rref = rref->next)
	{
	  if (rref->type != REF_COMPONENT)
	    continue;

	  rsym_pointer = rsym_pointer || rref->u.c.sym->attr.pointer;
	  rsym_target  = lsym_target  || rref->u.c.sym->attr.target;

	  if (symbols_could_alias (lref->u.c.sym, rref->u.c.sym,
				   lsym_pointer, lsym_target,
				   rsym_pointer, rsym_target))
	    return 1;

	  if ((lsym_pointer && (rsym_pointer || rsym_target))
	      || (rsym_pointer && (lsym_pointer || lsym_target)))
	    {
	      if (gfc_compare_types (&lref->u.c.component->ts,
				     &rref->u.c.sym->ts))
		return 1;
	      if (gfc_compare_types (&lref->u.c.sym->ts,
				     &rref->u.c.component->ts))
		return 1;
	      if (gfc_compare_types (&lref->u.c.component->ts,
				     &rref->u.c.component->ts))
		return 1;
	    }
	}
    }

  lsym_pointer = lsym->attr.pointer;
  lsym_target = lsym->attr.target;

  for (rref = rexpr->ref; rref != rss->info->data.array.ref; rref = rref->next)
    {
      if (rref->type != REF_COMPONENT)
	break;

      rsym_pointer = rsym_pointer || rref->u.c.sym->attr.pointer;
      rsym_target  = lsym_target  || rref->u.c.sym->attr.target;

      if (symbols_could_alias (rref->u.c.sym, lsym,
			       lsym_pointer, lsym_target,
			       rsym_pointer, rsym_target))
	return 1;

      if ((lsym_pointer && (rsym_pointer || rsym_target))
	  || (rsym_pointer && (lsym_pointer || lsym_target)))
	{
	  if (gfc_compare_types (&lsym->ts, &rref->u.c.component->ts))
	    return 1;
	}
    }

  return 0;
}


/* Resolve array data dependencies.  Creates a temporary if required.  */
/* TODO: Calc dependencies with gfc_expr rather than gfc_ss, and move to
   dependency.cc.  */

void
gfc_conv_resolve_dependencies (gfc_loopinfo * loop, gfc_ss * dest,
			       gfc_ss * rss)
{
  gfc_ss *ss;
  gfc_ref *lref;
  gfc_ref *rref;
  gfc_ss_info *ss_info;
  gfc_expr *dest_expr;
  gfc_expr *ss_expr;
  int nDepend = 0;
  int i, j;

  loop->temp_ss = NULL;
  dest_expr = dest->info->expr;

  for (ss = rss; ss != gfc_ss_terminator; ss = ss->next)
    {
      ss_info = ss->info;
      ss_expr = ss_info->expr;

      if (ss_info->array_outer_dependency)
	{
	  nDepend = 1;
	  break;
	}

      if (ss_info->type != GFC_SS_SECTION)
	{
	  if (flag_realloc_lhs
	      && dest_expr != ss_expr
	      && gfc_is_reallocatable_lhs (dest_expr)
	      && ss_expr->rank)
	    nDepend = gfc_check_dependency (dest_expr, ss_expr, true);

	  /* Check for cases like   c(:)(1:2) = c(2)(2:3)  */
	  if (!nDepend && dest_expr->rank > 0
	      && dest_expr->ts.type == BT_CHARACTER
	      && ss_expr->expr_type == EXPR_VARIABLE)

	    nDepend = gfc_check_dependency (dest_expr, ss_expr, false);

	  if (ss_info->type == GFC_SS_REFERENCE
	      && gfc_check_dependency (dest_expr, ss_expr, false))
	    ss_info->data.scalar.needs_temporary = 1;

	  if (nDepend)
	    break;
	  else
	    continue;
	}

      if (dest_expr->symtree->n.sym != ss_expr->symtree->n.sym)
	{
	  if (gfc_could_be_alias (dest, ss)
	      || gfc_are_equivalenced_arrays (dest_expr, ss_expr))
	    {
	      nDepend = 1;
	      break;
	    }
	}
      else
	{
	  lref = dest_expr->ref;
	  rref = ss_expr->ref;

	  nDepend = gfc_dep_resolver (lref, rref, &loop->reverse[0]);

	  if (nDepend == 1)
	    break;

	  for (i = 0; i < dest->dimen; i++)
	    for (j = 0; j < ss->dimen; j++)
	      if (i != j
		  && dest->dim[i] == ss->dim[j])
		{
		  /* If we don't access array elements in the same order,
		     there is a dependency.  */
		  nDepend = 1;
		  goto temporary;
		}
#if 0
	  /* TODO : loop shifting.  */
	  if (nDepend == 1)
	    {
	      /* Mark the dimensions for LOOP SHIFTING */
	      for (n = 0; n < loop->dimen; n++)
	        {
	          int dim = dest->data.info.dim[n];

		  if (lref->u.ar.dimen_type[dim] == DIMEN_VECTOR)
		    depends[n] = 2;
		  else if (! gfc_is_same_range (&lref->u.ar,
						&rref->u.ar, dim, 0))
		    depends[n] = 1;
	         }

	      /* Put all the dimensions with dependencies in the
		 innermost loops.  */
	      dim = 0;
	      for (n = 0; n < loop->dimen; n++)
		{
		  gcc_assert (loop->order[n] == n);
		  if (depends[n])
		  loop->order[dim++] = n;
		}
	      for (n = 0; n < loop->dimen; n++)
	        {
		  if (! depends[n])
		  loop->order[dim++] = n;
		}

	      gcc_assert (dim == loop->dimen);
	      break;
	    }
#endif
	}
    }

temporary:

  if (nDepend == 1)
    {
      tree base_type = gfc_typenode_for_spec (&dest_expr->ts);
      if (GFC_ARRAY_TYPE_P (base_type)
	  || GFC_DESCRIPTOR_TYPE_P (base_type))
	base_type = gfc_get_element_type (base_type);
      loop->temp_ss = gfc_get_temp_ss (base_type, dest->info->string_length,
				       loop->dimen);
      gfc_add_ss_to_loop (loop, loop->temp_ss);
    }
  else
    loop->temp_ss = NULL;
}


/* Browse through each array's information from the scalarizer and set the loop
   bounds according to the "best" one (per dimension), i.e. the one which
   provides the most information (constant bounds, shape, etc.).  */

static void
set_loop_bounds (gfc_loopinfo *loop)
{
  int n, dim, spec_dim;
  gfc_array_info *info;
  gfc_array_info *specinfo;
  gfc_ss *ss;
  tree tmp;
  gfc_ss **loopspec;
  bool dynamic[GFC_MAX_DIMENSIONS];
  mpz_t *cshape;
  mpz_t i;
  bool nonoptional_arr;

  gfc_loopinfo * const outer_loop = outermost_loop (loop);

  loopspec = loop->specloop;

  mpz_init (i);
  for (n = 0; n < loop->dimen; n++)
    {
      loopspec[n] = NULL;
      dynamic[n] = false;

      /* If there are both optional and nonoptional array arguments, scalarize
	 over the nonoptional; otherwise, it does not matter as then all
	 (optional) arrays have to be present per F2008, 125.2.12p3(6).  */

      nonoptional_arr = false;

      for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
	if (ss->info->type != GFC_SS_SCALAR && ss->info->type != GFC_SS_TEMP
	    && ss->info->type != GFC_SS_REFERENCE && !ss->info->can_be_null_ref)
	  {
	    nonoptional_arr = true;
	    break;
	  }

      /* We use one SS term, and use that to determine the bounds of the
	 loop for this dimension.  We try to pick the simplest term.  */
      for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
	{
	  gfc_ss_type ss_type;

	  ss_type = ss->info->type;
	  if (ss_type == GFC_SS_SCALAR
	      || ss_type == GFC_SS_TEMP
	      || ss_type == GFC_SS_REFERENCE
	      || (ss->info->can_be_null_ref && nonoptional_arr))
	    continue;

	  info = &ss->info->data.array;
	  dim = ss->dim[n];

	  if (loopspec[n] != NULL)
	    {
	      specinfo = &loopspec[n]->info->data.array;
	      spec_dim = loopspec[n]->dim[n];
	    }
	  else
	    {
	      /* Silence uninitialized warnings.  */
	      specinfo = NULL;
	      spec_dim = 0;
	    }

	  if (info->shape)
	    {
	      /* The frontend has worked out the size for us.  */
	      if (!loopspec[n]
		  || !specinfo->shape
		  || !integer_zerop (specinfo->start[spec_dim]))
		/* Prefer zero-based descriptors if possible.  */
		loopspec[n] = ss;
	      continue;
	    }

	  if (ss_type == GFC_SS_CONSTRUCTOR)
	    {
	      gfc_constructor_base base;
	      /* An unknown size constructor will always be rank one.
		 Higher rank constructors will either have known shape,
		 or still be wrapped in a call to reshape.  */
	      gcc_assert (loop->dimen == 1);

	      /* Always prefer to use the constructor bounds if the size
		 can be determined at compile time.  Prefer not to otherwise,
		 since the general case involves realloc, and it's better to
		 avoid that overhead if possible.  */
	      base = ss->info->expr->value.constructor;
	      dynamic[n] = gfc_get_array_constructor_size (&i, base);
	      if (!dynamic[n] || !loopspec[n])
		loopspec[n] = ss;
	      continue;
	    }

	  /* Avoid using an allocatable lhs in an assignment, since
	     there might be a reallocation coming.  */
	  if (loopspec[n] && ss->is_alloc_lhs)
	    continue;

	  if (!loopspec[n])
	    loopspec[n] = ss;
	  /* Criteria for choosing a loop specifier (most important first):
	     doesn't need realloc
	     stride of one
	     known stride
	     known lower bound
	     known upper bound
	   */
	  else if (loopspec[n]->info->type == GFC_SS_CONSTRUCTOR && dynamic[n])
	    loopspec[n] = ss;
	  else if (integer_onep (info->stride[dim])
		   && !integer_onep (specinfo->stride[spec_dim]))
	    loopspec[n] = ss;
	  else if (INTEGER_CST_P (info->stride[dim])
		   && !INTEGER_CST_P (specinfo->stride[spec_dim]))
	    loopspec[n] = ss;
	  else if (INTEGER_CST_P (info->start[dim])
		   && !INTEGER_CST_P (specinfo->start[spec_dim])
		   && integer_onep (info->stride[dim])
		      == integer_onep (specinfo->stride[spec_dim])
		   && INTEGER_CST_P (info->stride[dim])
		      == INTEGER_CST_P (specinfo->stride[spec_dim]))
	    loopspec[n] = ss;
	  /* We don't work out the upper bound.
	     else if (INTEGER_CST_P (info->finish[n])
	     && ! INTEGER_CST_P (specinfo->finish[n]))
	     loopspec[n] = ss; */
	}

      /* We should have found the scalarization loop specifier.  If not,
	 that's bad news.  */
      gcc_assert (loopspec[n]);

      info = &loopspec[n]->info->data.array;
      dim = loopspec[n]->dim[n];

      /* Set the extents of this range.  */
      cshape = info->shape;
      if (cshape && INTEGER_CST_P (info->start[dim])
	  && INTEGER_CST_P (info->stride[dim]))
	{
	  loop->from[n] = info->start[dim];
	  mpz_set (i, cshape[get_array_ref_dim_for_loop_dim (loopspec[n], n)]);
	  mpz_sub_ui (i, i, 1);
	  /* To = from + (size - 1) * stride.  */
	  tmp = gfc_conv_mpz_to_tree (i, gfc_index_integer_kind);
	  if (!integer_onep (info->stride[dim]))
	    tmp = fold_build2_loc (input_location, MULT_EXPR,
				   gfc_array_index_type, tmp,
				   info->stride[dim]);
	  loop->to[n] = fold_build2_loc (input_location, PLUS_EXPR,
					 gfc_array_index_type,
					 loop->from[n], tmp);
	}
      else
	{
	  loop->from[n] = info->start[dim];
	  switch (loopspec[n]->info->type)
	    {
	    case GFC_SS_CONSTRUCTOR:
	      /* The upper bound is calculated when we expand the
		 constructor.  */
	      gcc_assert (loop->to[n] == NULL_TREE);
	      break;

	    case GFC_SS_SECTION:
	      /* Use the end expression if it exists and is not constant,
		 so that it is only evaluated once.  */
	      loop->to[n] = info->end[dim];
	      break;

	    case GFC_SS_FUNCTION:
	      /* The loop bound will be set when we generate the call.  */
	      gcc_assert (loop->to[n] == NULL_TREE);
	      break;

	    case GFC_SS_INTRINSIC:
	      {
		gfc_expr *expr = loopspec[n]->info->expr;

		/* The {l,u}bound of an assumed rank.  */
		if (expr->value.function.isym->id == GFC_ISYM_SHAPE)
		  gcc_assert (expr->value.function.actual->expr->rank == -1);
		else
		  gcc_assert ((expr->value.function.isym->id == GFC_ISYM_LBOUND
			       || expr->value.function.isym->id == GFC_ISYM_UBOUND)
			      && expr->value.function.actual->next->expr == NULL
			      && expr->value.function.actual->expr->rank == -1);

		loop->to[n] = info->end[dim];
		break;
	      }

	    case GFC_SS_COMPONENT:
	      {
		if (info->end[dim] != NULL_TREE)
		  {
		    loop->to[n] = info->end[dim];
		    break;
		  }
		else
		  gcc_unreachable ();
	      }

	    default:
	      gcc_unreachable ();
	    }
	}

      /* Transform everything so we have a simple incrementing variable.  */
      if (integer_onep (info->stride[dim]))
	info->delta[dim] = gfc_index_zero_node;
      else
	{
	  /* Set the delta for this section.  */
	  info->delta[dim] = gfc_evaluate_now (loop->from[n], &outer_loop->pre);
	  /* Number of iterations is (end - start + step) / step.
	     with start = 0, this simplifies to
	     last = end / step;
	     for (i = 0; i<=last; i++){...};  */
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type, loop->to[n],
				 loop->from[n]);
	  tmp = fold_build2_loc (input_location, FLOOR_DIV_EXPR,
				 gfc_array_index_type, tmp, info->stride[dim]);
	  tmp = fold_build2_loc (input_location, MAX_EXPR, gfc_array_index_type,
				 tmp, build_int_cst (gfc_array_index_type, -1));
	  loop->to[n] = gfc_evaluate_now (tmp, &outer_loop->pre);
	  /* Make the loop variable start at 0.  */
	  loop->from[n] = gfc_index_zero_node;
	}
    }
  mpz_clear (i);

  for (loop = loop->nested; loop; loop = loop->next)
    set_loop_bounds (loop);
}


/* Initialize the scalarization loop.  Creates the loop variables.  Determines
   the range of the loop variables.  Creates a temporary if required.
   Also generates code for scalar expressions which have been
   moved outside the loop.  */

void
gfc_conv_loop_setup (gfc_loopinfo * loop, locus * where)
{
  gfc_ss *tmp_ss;
  tree tmp;

  set_loop_bounds (loop);

  /* Add all the scalar code that can be taken out of the loops.
     This may include calculating the loop bounds, so do it before
     allocating the temporary.  */
  gfc_add_loop_ss_code (loop, loop->ss, false, where);

  tmp_ss = loop->temp_ss;
  /* If we want a temporary then create it.  */
  if (tmp_ss != NULL)
    {
      gfc_ss_info *tmp_ss_info;

      tmp_ss_info = tmp_ss->info;
      gcc_assert (tmp_ss_info->type == GFC_SS_TEMP);
      gcc_assert (loop->parent == NULL);

      /* Make absolutely sure that this is a complete type.  */
      if (tmp_ss_info->string_length)
	tmp_ss_info->data.temp.type
		= gfc_get_character_type_len_for_eltype
			(TREE_TYPE (tmp_ss_info->data.temp.type),
			 tmp_ss_info->string_length);

      tmp = tmp_ss_info->data.temp.type;
      memset (&tmp_ss_info->data.array, 0, sizeof (gfc_array_info));
      tmp_ss_info->type = GFC_SS_SECTION;

      gcc_assert (tmp_ss->dimen != 0);

      gfc_trans_create_temp_array (&loop->pre, &loop->post, tmp_ss, tmp,
				   NULL_TREE, false, true, false, where);
    }

  /* For array parameters we don't have loop variables, so don't calculate the
     translations.  */
  if (!loop->array_parameter)
    gfc_set_delta (loop);
}


/* Calculates how to transform from loop variables to array indices for each
   array: once loop bounds are chosen, sets the difference (DELTA field) between
   loop bounds and array reference bounds, for each array info.  */

void
gfc_set_delta (gfc_loopinfo *loop)
{
  gfc_ss *ss, **loopspec;
  gfc_array_info *info;
  tree tmp;
  int n, dim;

  gfc_loopinfo * const outer_loop = outermost_loop (loop);

  loopspec = loop->specloop;

  /* Calculate the translation from loop variables to array indices.  */
  for (ss = loop->ss; ss != gfc_ss_terminator; ss = ss->loop_chain)
    {
      gfc_ss_type ss_type;

      ss_type = ss->info->type;
      if (ss_type != GFC_SS_SECTION
	  && ss_type != GFC_SS_COMPONENT
	  && ss_type != GFC_SS_CONSTRUCTOR)
	continue;

      info = &ss->info->data.array;

      for (n = 0; n < ss->dimen; n++)
	{
	  /* If we are specifying the range the delta is already set.  */
	  if (loopspec[n] != ss)
	    {
	      dim = ss->dim[n];

	      /* Calculate the offset relative to the loop variable.
		 First multiply by the stride.  */
	      tmp = loop->from[n];
	      if (!integer_onep (info->stride[dim]))
		tmp = fold_build2_loc (input_location, MULT_EXPR,
				       gfc_array_index_type,
				       tmp, info->stride[dim]);

	      /* Then subtract this from our starting value.  */
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     gfc_array_index_type,
				     info->start[dim], tmp);

	      info->delta[dim] = gfc_evaluate_now (tmp, &outer_loop->pre);
	    }
	}
    }

  for (loop = loop->nested; loop; loop = loop->next)
    gfc_set_delta (loop);
}


/* Calculate the size of a given array dimension from the bounds.  This
   is simply (ubound - lbound + 1) if this expression is positive
   or 0 if it is negative (pick either one if it is zero).  Optionally
   (if or_expr is present) OR the (expression != 0) condition to it.  */

tree
gfc_conv_array_extent_dim (tree lbound, tree ubound, tree* or_expr)
{
  tree res;
  tree cond;

  /* Calculate (ubound - lbound + 1).  */
  res = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			 ubound, lbound);
  res = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type, res,
			 gfc_index_one_node);

  /* Check whether the size for this dimension is negative.  */
  cond = fold_build2_loc (input_location, LE_EXPR, logical_type_node, res,
			  gfc_index_zero_node);
  res = fold_build3_loc (input_location, COND_EXPR, gfc_array_index_type, cond,
			 gfc_index_zero_node, res);

  /* Build OR expression.  */
  if (or_expr)
    *or_expr = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				logical_type_node, *or_expr, cond);

  return res;
}


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


/* Fills in an array descriptor, and returns the size of the array.
   The size will be a simple_val, ie a variable or a constant.  Also
   calculates the offset of the base.  The pointer argument overflow,
   which should be of integer type, will increase in value if overflow
   occurs during the size calculation.  Returns the size of the array.
   {
    stride = 1;
    offset = 0;
    for (n = 0; n < rank; n++)
      {
	a.lbound[n] = specified_lower_bound;
	offset = offset + a.lbond[n] * stride;
	size = 1 - lbound;
	a.ubound[n] = specified_upper_bound;
	a.stride[n] = stride;
	size = size >= 0 ? ubound + size : 0; //size = ubound + 1 - lbound
	overflow += size == 0 ? 0: (MAX/size < stride ? 1: 0);
	stride = stride * size;
      }
    for (n = rank; n < rank+corank; n++)
      (Set lcobound/ucobound as above.)
    element_size = sizeof (array element);
    if (!rank)
      return element_size
    stride = (size_t) stride;
    overflow += element_size == 0 ? 0: (MAX/element_size < stride ? 1: 0);
    stride = stride * element_size;
    return (stride);
   }  */
/*GCC ARRAYS*/

static tree
gfc_array_init_size (tree descriptor, int rank, int corank, tree * poffset,
		     gfc_expr ** lower, gfc_expr ** upper, stmtblock_t * pblock,
		     stmtblock_t * descriptor_block, tree * overflow,
		     tree expr3_elem_size, tree *nelems, gfc_expr *expr3,
		     tree expr3_desc, bool e3_has_nodescriptor, gfc_expr *expr,
		     tree *element_size)
{
  tree type;
  tree tmp;
  tree size;
  tree offset;
  tree stride;
  tree or_expr;
  tree thencase;
  tree elsecase;
  tree cond;
  tree var;
  stmtblock_t thenblock;
  stmtblock_t elseblock;
  gfc_expr *ubound;
  gfc_se se;
  int n;

  type = TREE_TYPE (descriptor);

  stride = gfc_index_one_node;
  offset = gfc_index_zero_node;

  /* Set the dtype before the alloc, because registration of coarrays needs
     it initialized.  */
  if (expr->ts.type == BT_CHARACTER
      && expr->ts.deferred
      && VAR_P (expr->ts.u.cl->backend_decl))
    {
      type = gfc_typenode_for_spec (&expr->ts);
      tmp = gfc_conv_descriptor_dtype (descriptor);
      gfc_add_modify (pblock, tmp, gfc_get_dtype_rank_type (rank, type));
    }
  else if (expr->ts.type == BT_CHARACTER
	   && expr->ts.deferred
	   && TREE_CODE (descriptor) == COMPONENT_REF)
    {
      /* Deferred character components have their string length tucked away
	 in a hidden field of the derived type. Obtain that and use it to
	 set the dtype. The charlen backend decl is zero because the field
	 type is zero length.  */
      gfc_ref *ref;
      tmp = NULL_TREE;
      for (ref = expr->ref; ref; ref = ref->next)
	if (ref->type == REF_COMPONENT
	    && gfc_deferred_strlen (ref->u.c.component, &tmp))
	  break;
      gcc_assert (tmp != NULL_TREE);
      tmp = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			     TREE_OPERAND (descriptor, 0), tmp, NULL_TREE);
      tmp = fold_convert (gfc_charlen_type_node, tmp);
      type = gfc_get_character_type_len (expr->ts.kind, tmp);
      tmp = gfc_conv_descriptor_dtype (descriptor);
      gfc_add_modify (pblock, tmp, gfc_get_dtype_rank_type (rank, type));
    }
  else if (expr3_desc && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (expr3_desc)))
    {
      tmp = gfc_conv_descriptor_dtype (descriptor);
      gfc_add_modify (pblock, tmp, gfc_conv_descriptor_dtype (expr3_desc));
    }
  else
    {
      tmp = gfc_conv_descriptor_dtype (descriptor);
      gfc_add_modify (pblock, tmp, gfc_get_dtype (type));
    }

  or_expr = logical_false_node;

  for (n = 0; n < rank; n++)
    {
      tree conv_lbound;
      tree conv_ubound;

      /* We have 3 possibilities for determining the size of the array:
	 lower == NULL    => lbound = 1, ubound = upper[n]
	 upper[n] = NULL  => lbound = 1, ubound = lower[n]
	 upper[n] != NULL => lbound = lower[n], ubound = upper[n]  */
      ubound = upper[n];

      /* Set lower bound.  */
      gfc_init_se (&se, NULL);
      if (expr3_desc != NULL_TREE)
	{
	  if (e3_has_nodescriptor)
	    /* The lbound of nondescriptor arrays like array constructors,
	       nonallocatable/nonpointer function results/variables,
	       start at zero, but when allocating it, the standard expects
	       the array to start at one.  */
	    se.expr = gfc_index_one_node;
	  else
	    se.expr = gfc_conv_descriptor_lbound_get (expr3_desc,
						      gfc_rank_cst[n]);
	}
      else if (lower == NULL)
	se.expr = gfc_index_one_node;
      else
	{
	  gcc_assert (lower[n]);
	  if (ubound)
	    {
	      gfc_conv_expr_type (&se, lower[n], gfc_array_index_type);
	      gfc_add_block_to_block (pblock, &se.pre);
	    }
	  else
	    {
	      se.expr = gfc_index_one_node;
	      ubound = lower[n];
	    }
	}
      gfc_conv_descriptor_lbound_set (descriptor_block, descriptor,
				      gfc_rank_cst[n], se.expr);
      conv_lbound = se.expr;

      /* Work out the offset for this component.  */
      tmp = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			     se.expr, stride);
      offset = fold_build2_loc (input_location, MINUS_EXPR,
				gfc_array_index_type, offset, tmp);

      /* Set upper bound.  */
      gfc_init_se (&se, NULL);
      if (expr3_desc != NULL_TREE)
	{
	  if (e3_has_nodescriptor)
	    {
	      /* The lbound of nondescriptor arrays like array constructors,
		 nonallocatable/nonpointer function results/variables,
		 start at zero, but when allocating it, the standard expects
		 the array to start at one.  Therefore fix the upper bound to be
		 (desc.ubound - desc.lbound) + 1.  */
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     gfc_array_index_type,
				     gfc_conv_descriptor_ubound_get (
				       expr3_desc, gfc_rank_cst[n]),
				     gfc_conv_descriptor_lbound_get (
				       expr3_desc, gfc_rank_cst[n]));
	      tmp = fold_build2_loc (input_location, PLUS_EXPR,
				     gfc_array_index_type, tmp,
				     gfc_index_one_node);
	      se.expr = gfc_evaluate_now (tmp, pblock);
	    }
	  else
	    se.expr = gfc_conv_descriptor_ubound_get (expr3_desc,
						      gfc_rank_cst[n]);
	}
      else
	{
	  gcc_assert (ubound);
	  gfc_conv_expr_type (&se, ubound, gfc_array_index_type);
	  gfc_add_block_to_block (pblock, &se.pre);
	  if (ubound->expr_type == EXPR_FUNCTION)
	    se.expr = gfc_evaluate_now (se.expr, pblock);
	}
      gfc_conv_descriptor_ubound_set (descriptor_block, descriptor,
				      gfc_rank_cst[n], se.expr);
      conv_ubound = se.expr;

      /* Store the stride.  */
      gfc_conv_descriptor_stride_set (descriptor_block, descriptor,
				      gfc_rank_cst[n], stride);

      /* Calculate size and check whether extent is negative.  */
      size = gfc_conv_array_extent_dim (conv_lbound, conv_ubound, &or_expr);
      size = gfc_evaluate_now (size, pblock);

      /* Check whether multiplying the stride by the number of
	 elements in this dimension would overflow. We must also check
	 whether the current dimension has zero size in order to avoid
	 division by zero.
      */
      tmp = fold_build2_loc (input_location, TRUNC_DIV_EXPR,
			     gfc_array_index_type,
			     fold_convert (gfc_array_index_type,
					   TYPE_MAX_VALUE (gfc_array_index_type)),
					   size);
      cond = gfc_unlikely (fold_build2_loc (input_location, LT_EXPR,
					    logical_type_node, tmp, stride),
			   PRED_FORTRAN_OVERFLOW);
      tmp = fold_build3_loc (input_location, COND_EXPR, integer_type_node, cond,
			     integer_one_node, integer_zero_node);
      cond = gfc_unlikely (fold_build2_loc (input_location, EQ_EXPR,
					    logical_type_node, size,
					    gfc_index_zero_node),
			   PRED_FORTRAN_SIZE_ZERO);
      tmp = fold_build3_loc (input_location, COND_EXPR, integer_type_node, cond,
			     integer_zero_node, tmp);
      tmp = fold_build2_loc (input_location, PLUS_EXPR, integer_type_node,
			     *overflow, tmp);
      *overflow = gfc_evaluate_now (tmp, pblock);

      /* Multiply the stride by the number of elements in this dimension.  */
      stride = fold_build2_loc (input_location, MULT_EXPR,
				gfc_array_index_type, stride, size);
      stride = gfc_evaluate_now (stride, pblock);
    }

  for (n = rank; n < rank + corank; n++)
    {
      ubound = upper[n];

      /* Set lower bound.  */
      gfc_init_se (&se, NULL);
      if (lower == NULL || lower[n] == NULL)
	{
	  gcc_assert (n == rank + corank - 1);
	  se.expr = gfc_index_one_node;
	}
      else
	{
	  if (ubound || n == rank + corank - 1)
	    {
	      gfc_conv_expr_type (&se, lower[n], gfc_array_index_type);
	      gfc_add_block_to_block (pblock, &se.pre);
	    }
	  else
	    {
	      se.expr = gfc_index_one_node;
	      ubound = lower[n];
	    }
	}
      gfc_conv_descriptor_lbound_set (descriptor_block, descriptor,
				      gfc_rank_cst[n], se.expr);

      if (n < rank + corank - 1)
	{
	  gfc_init_se (&se, NULL);
	  gcc_assert (ubound);
	  gfc_conv_expr_type (&se, ubound, gfc_array_index_type);
	  gfc_add_block_to_block (pblock, &se.pre);
	  gfc_conv_descriptor_ubound_set (descriptor_block, descriptor,
					  gfc_rank_cst[n], se.expr);
	}
    }

  /* The stride is the number of elements in the array, so multiply by the
     size of an element to get the total size.  Obviously, if there is a
     SOURCE expression (expr3) we must use its element size.  */
  if (expr3_elem_size != NULL_TREE)
    tmp = expr3_elem_size;
  else if (expr3 != NULL)
    {
      if (expr3->ts.type == BT_CLASS)
	{
	  gfc_se se_sz;
	  gfc_expr *sz = gfc_copy_expr (expr3);
	  gfc_add_vptr_component (sz);
	  gfc_add_size_component (sz);
	  gfc_init_se (&se_sz, NULL);
	  gfc_conv_expr (&se_sz, sz);
	  gfc_free_expr (sz);
	  tmp = se_sz.expr;
	}
      else
	{
	  tmp = gfc_typenode_for_spec (&expr3->ts);
	  tmp = TYPE_SIZE_UNIT (tmp);
	}
    }
  else
    tmp = TYPE_SIZE_UNIT (gfc_get_element_type (type));

  /* Convert to size_t.  */
  *element_size = fold_convert (size_type_node, tmp);

  if (rank == 0)
    return *element_size;

  *nelems = gfc_evaluate_now (stride, pblock);
  stride = fold_convert (size_type_node, stride);

  /* First check for overflow. Since an array of type character can
     have zero element_size, we must check for that before
     dividing.  */
  tmp = fold_build2_loc (input_location, TRUNC_DIV_EXPR,
			 size_type_node,
			 TYPE_MAX_VALUE (size_type_node), *element_size);
  cond = gfc_unlikely (fold_build2_loc (input_location, LT_EXPR,
					logical_type_node, tmp, stride),
		       PRED_FORTRAN_OVERFLOW);
  tmp = fold_build3_loc (input_location, COND_EXPR, integer_type_node, cond,
			 integer_one_node, integer_zero_node);
  cond = gfc_unlikely (fold_build2_loc (input_location, EQ_EXPR,
					logical_type_node, *element_size,
					build_int_cst (size_type_node, 0)),
		       PRED_FORTRAN_SIZE_ZERO);
  tmp = fold_build3_loc (input_location, COND_EXPR, integer_type_node, cond,
			 integer_zero_node, tmp);
  tmp = fold_build2_loc (input_location, PLUS_EXPR, integer_type_node,
			 *overflow, tmp);
  *overflow = gfc_evaluate_now (tmp, pblock);

  size = fold_build2_loc (input_location, MULT_EXPR, size_type_node,
			  stride, *element_size);

  if (poffset != NULL)
    {
      offset = gfc_evaluate_now (offset, pblock);
      *poffset = offset;
    }

  if (integer_zerop (or_expr))
    return size;
  if (integer_onep (or_expr))
    return build_int_cst (size_type_node, 0);

  var = gfc_create_var (TREE_TYPE (size), "size");
  gfc_start_block (&thenblock);
  gfc_add_modify (&thenblock, var, build_int_cst (size_type_node, 0));
  thencase = gfc_finish_block (&thenblock);

  gfc_start_block (&elseblock);
  gfc_add_modify (&elseblock, var, size);
  elsecase = gfc_finish_block (&elseblock);

  tmp = gfc_evaluate_now (or_expr, pblock);
  tmp = build3_v (COND_EXPR, tmp, thencase, elsecase);
  gfc_add_expr_to_block (pblock, tmp);

  return var;
}


/* Retrieve the last ref from the chain.  This routine is specific to
   gfc_array_allocate ()'s needs.  */

bool
retrieve_last_ref (gfc_ref **ref_in, gfc_ref **prev_ref_in)
{
  gfc_ref *ref, *prev_ref;

  ref = *ref_in;
  /* Prevent warnings for uninitialized variables.  */
  prev_ref = *prev_ref_in;
  while (ref && ref->next != NULL)
    {
      gcc_assert (ref->type != REF_ARRAY || ref->u.ar.type == AR_ELEMENT
		  || (ref->u.ar.dimen == 0 && ref->u.ar.codimen > 0));
      prev_ref = ref;
      ref = ref->next;
    }

  if (ref == NULL || ref->type != REF_ARRAY)
    return false;

  *ref_in = ref;
  *prev_ref_in = prev_ref;
  return true;
}

/* Initializes the descriptor and generates a call to _gfor_allocate.  Does
   the work for an ALLOCATE statement.  */
/*GCC ARRAYS*/

bool
gfc_array_allocate (gfc_se * se, gfc_expr * expr, tree status, tree errmsg,
		    tree errlen, tree label_finish, tree expr3_elem_size,
		    tree *nelems, gfc_expr *expr3, tree e3_arr_desc,
		    bool e3_has_nodescriptor, gfc_omp_namelist *omp_alloc)
{
  tree tmp;
  tree pointer;
  tree offset = NULL_TREE;
  tree token = NULL_TREE;
  tree size;
  tree msg;
  tree error = NULL_TREE;
  tree overflow; /* Boolean storing whether size calculation overflows.  */
  tree var_overflow = NULL_TREE;
  tree cond;
  tree set_descriptor;
  tree not_prev_allocated = NULL_TREE;
  tree element_size = NULL_TREE;
  stmtblock_t set_descriptor_block;
  stmtblock_t elseblock;
  gfc_expr **lower;
  gfc_expr **upper;
  gfc_ref *ref, *prev_ref = NULL, *coref;
  bool allocatable, coarray, dimension, alloc_w_e3_arr_spec = false,
      non_ulimate_coarray_ptr_comp;
  tree omp_cond = NULL_TREE, omp_alt_alloc = NULL_TREE;

  ref = expr->ref;

  /* Find the last reference in the chain.  */
  if (!retrieve_last_ref (&ref, &prev_ref))
    return false;

  /* Take the allocatable and coarray properties solely from the expr-ref's
     attributes and not from source=-expression.  */
  if (!prev_ref)
    {
      allocatable = expr->symtree->n.sym->attr.allocatable;
      dimension = expr->symtree->n.sym->attr.dimension;
      non_ulimate_coarray_ptr_comp = false;
    }
  else
    {
      allocatable = prev_ref->u.c.component->attr.allocatable;
      /* Pointer components in coarrayed derived types must be treated
	 specially in that they are registered without a check if the are
	 already associated.  This does not hold for ultimate coarray
	 pointers.  */
      non_ulimate_coarray_ptr_comp = (prev_ref->u.c.component->attr.pointer
	      && !prev_ref->u.c.component->attr.codimension);
      dimension = prev_ref->u.c.component->attr.dimension;
    }

  /* For allocatable/pointer arrays in derived types, one of the refs has to be
     a coarray.  In this case it does not matter whether we are on this_image
     or not.  */
  coarray = false;
  for (coref = expr->ref; coref; coref = coref->next)
    if (coref->type == REF_ARRAY && coref->u.ar.codimen > 0)
      {
	coarray = true;
	break;
      }

  if (!dimension)
    gcc_assert (coarray);

  if (ref->u.ar.type == AR_FULL && expr3 != NULL)
    {
      gfc_ref *old_ref = ref;
      /* F08:C633: Array shape from expr3.  */
      ref = expr3->ref;

      /* Find the last reference in the chain.  */
      if (!retrieve_last_ref (&ref, &prev_ref))
	{
	  if (expr3->expr_type == EXPR_FUNCTION
	      && gfc_expr_attr (expr3).dimension)
	    ref = old_ref;
	  else
	    return false;
	}
      alloc_w_e3_arr_spec = true;
    }

  /* Figure out the size of the array.  */
  switch (ref->u.ar.type)
    {
    case AR_ELEMENT:
      if (!coarray)
	{
	  lower = NULL;
	  upper = ref->u.ar.start;
	  break;
	}
      /* Fall through.  */

    case AR_SECTION:
      lower = ref->u.ar.start;
      upper = ref->u.ar.end;
      break;

    case AR_FULL:
      gcc_assert (ref->u.ar.as->type == AS_EXPLICIT
		  || alloc_w_e3_arr_spec);

      lower = ref->u.ar.as->lower;
      upper = ref->u.ar.as->upper;
      break;

    default:
      gcc_unreachable ();
      break;
    }

  overflow = integer_zero_node;

  if (expr->ts.type == BT_CHARACTER
      && TREE_CODE (se->string_length) == COMPONENT_REF
      && expr->ts.u.cl->backend_decl != se->string_length
      && VAR_P (expr->ts.u.cl->backend_decl))
    gfc_add_modify (&se->pre, expr->ts.u.cl->backend_decl,
		    fold_convert (TREE_TYPE (expr->ts.u.cl->backend_decl),
				  se->string_length));

  gfc_init_block (&set_descriptor_block);
  /* Take the corank only from the actual ref and not from the coref.  The
     later will mislead the generation of the array dimensions for allocatable/
     pointer components in derived types.  */
  size = gfc_array_init_size (se->expr, alloc_w_e3_arr_spec ? expr->rank
							   : ref->u.ar.as->rank,
			      coarray ? ref->u.ar.as->corank : 0,
			      &offset, lower, upper,
			      &se->pre, &set_descriptor_block, &overflow,
			      expr3_elem_size, nelems, expr3, e3_arr_desc,
			      e3_has_nodescriptor, expr, &element_size);

  if (dimension)
    {
      var_overflow = gfc_create_var (integer_type_node, "overflow");
      gfc_add_modify (&se->pre, var_overflow, overflow);

      if (status == NULL_TREE)
	{
	  /* Generate the block of code handling overflow.  */
	  msg = gfc_build_addr_expr (pchar_type_node,
		    gfc_build_localized_cstring_const
  			("Integer overflow when calculating the amount of "
  			 "memory to allocate"));
	  error = build_call_expr_loc (input_location,
				       gfor_fndecl_runtime_error, 1, msg);
	}
      else
	{
	  tree status_type = TREE_TYPE (status);
	  stmtblock_t set_status_block;

	  gfc_start_block (&set_status_block);
	  gfc_add_modify (&set_status_block, status,
			  build_int_cst (status_type, LIBERROR_ALLOCATION));
	  error = gfc_finish_block (&set_status_block);
	}
    }

  /* Allocate memory to store the data.  */
  if (POINTER_TYPE_P (TREE_TYPE (se->expr)))
    se->expr = build_fold_indirect_ref_loc (input_location, se->expr);

  if (coarray && flag_coarray == GFC_FCOARRAY_LIB)
    {
      pointer = non_ulimate_coarray_ptr_comp ? se->expr
				      : gfc_conv_descriptor_data_get (se->expr);
      token = gfc_conv_descriptor_token (se->expr);
      token = gfc_build_addr_expr (NULL_TREE, token);
    }
  else
    {
      pointer = gfc_conv_descriptor_data_get (se->expr);
      if (omp_alloc)
	omp_cond = boolean_true_node;
    }
  STRIP_NOPS (pointer);

  if (allocatable)
    {
      not_prev_allocated = gfc_create_var (logical_type_node,
					   "not_prev_allocated");
      tmp = fold_build2_loc (input_location, EQ_EXPR,
			     logical_type_node, pointer,
			     build_int_cst (TREE_TYPE (pointer), 0));

      gfc_add_modify (&se->pre, not_prev_allocated, tmp);
    }

  gfc_start_block (&elseblock);

  tree succ_add_expr = NULL_TREE;
  if (omp_cond)
    {
      tree align, alloc, sz;
      gfc_se se2;
      if (omp_alloc->u2.allocator)
	{
	  gfc_init_se (&se2, NULL);
	  gfc_conv_expr (&se2, omp_alloc->u2.allocator);
	  gfc_add_block_to_block (&elseblock, &se2.pre);
	  alloc = gfc_evaluate_now (se2.expr, &elseblock);
	  gfc_add_block_to_block (&elseblock, &se2.post);
	}
      else
	alloc = build_zero_cst (ptr_type_node);
      tmp = TREE_TYPE (TREE_TYPE (pointer));
      if (tmp == void_type_node)
	tmp = gfc_typenode_for_spec (&expr->ts, 0);
      if (omp_alloc->u.align)
	{
	  gfc_init_se (&se2, NULL);
	  gfc_conv_expr (&se2, omp_alloc->u.align);
	  gcc_assert (CONSTANT_CLASS_P (se2.expr)
		      && se2.pre.head == NULL
		      && se2.post.head == NULL);
	  align = build_int_cst (size_type_node,
				 MAX (tree_to_uhwi (se2.expr),
				      TYPE_ALIGN_UNIT (tmp)));
	}
      else
	align = build_int_cst (size_type_node, TYPE_ALIGN_UNIT (tmp));
      sz = fold_build2_loc (input_location, MAX_EXPR, size_type_node,
			    fold_convert (size_type_node, size),
			    build_int_cst (size_type_node, 1));
      omp_alt_alloc = builtin_decl_explicit (BUILT_IN_GOMP_ALLOC);
      DECL_ATTRIBUTES (omp_alt_alloc)
	= tree_cons (get_identifier ("omp allocator"),
		     build_tree_list (NULL_TREE, alloc),
		     DECL_ATTRIBUTES (omp_alt_alloc));
      omp_alt_alloc = build_call_expr (omp_alt_alloc, 3, align, sz, alloc);
      succ_add_expr = fold_build2_loc (input_location, MODIFY_EXPR,
				       void_type_node,
				       gfc_conv_descriptor_version (se->expr),
				       build_int_cst (integer_type_node, 1));
    }

  /* The allocatable variant takes the old pointer as first argument.  */
  if (allocatable)
    gfc_allocate_allocatable (&elseblock, pointer, size, token,
			      status, errmsg, errlen, label_finish, expr,
			      coref != NULL ? coref->u.ar.as->corank : 0,
			      omp_cond, omp_alt_alloc, succ_add_expr);
  else if (non_ulimate_coarray_ptr_comp && token)
    /* The token is set only for GFC_FCOARRAY_LIB mode.  */
    gfc_allocate_using_caf_lib (&elseblock, pointer, size, token, status,
				errmsg, errlen,
				GFC_CAF_COARRAY_ALLOC_ALLOCATE_ONLY);
  else
    gfc_allocate_using_malloc (&elseblock, pointer, size, status,
			       omp_cond, omp_alt_alloc, succ_add_expr);

  if (dimension)
    {
      cond = gfc_unlikely (fold_build2_loc (input_location, NE_EXPR,
			   logical_type_node, var_overflow, integer_zero_node),
			   PRED_FORTRAN_OVERFLOW);
      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, cond,
			     error, gfc_finish_block (&elseblock));
    }
  else
    tmp = gfc_finish_block (&elseblock);

  gfc_add_expr_to_block (&se->pre, tmp);

  /* Update the array descriptor with the offset and the span.  */
  if (dimension)
    {
      gfc_conv_descriptor_offset_set (&set_descriptor_block, se->expr, offset);
      tmp = fold_convert (gfc_array_index_type, element_size);
      gfc_conv_descriptor_span_set (&set_descriptor_block, se->expr, tmp);
    }

  set_descriptor = gfc_finish_block (&set_descriptor_block);
  if (status != NULL_TREE)
    {
      cond = fold_build2_loc (input_location, EQ_EXPR,
			  logical_type_node, status,
			  build_int_cst (TREE_TYPE (status), 0));

      if (not_prev_allocated != NULL_TREE)
	cond = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				logical_type_node, cond, not_prev_allocated);

      gfc_add_expr_to_block (&se->pre,
		 fold_build3_loc (input_location, COND_EXPR, void_type_node,
				  cond,
				  set_descriptor,
				  build_empty_stmt (input_location)));
    }
  else
      gfc_add_expr_to_block (&se->pre, set_descriptor);

  return true;
}


/* Create an array constructor from an initialization expression.
   We assume the frontend already did any expansions and conversions.  */

tree
gfc_conv_array_initializer (tree type, gfc_expr * expr)
{
  gfc_constructor *c;
  tree tmp;
  gfc_se se;
  tree index, range;
  vec<constructor_elt, va_gc> *v = NULL;

  if (expr->expr_type == EXPR_VARIABLE
      && expr->symtree->n.sym->attr.flavor == FL_PARAMETER
      && expr->symtree->n.sym->value)
    expr = expr->symtree->n.sym->value;

  switch (expr->expr_type)
    {
    case EXPR_CONSTANT:
    case EXPR_STRUCTURE:
      /* A single scalar or derived type value.  Create an array with all
         elements equal to that value.  */
      gfc_init_se (&se, NULL);

      if (expr->expr_type == EXPR_CONSTANT)
	gfc_conv_constant (&se, expr);
      else
	gfc_conv_structure (&se, expr, 1);

      if (tree_int_cst_lt (TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
			   TYPE_MIN_VALUE (TYPE_DOMAIN (type))))
	break;
      else if (tree_int_cst_equal (TYPE_MIN_VALUE (TYPE_DOMAIN (type)),
				   TYPE_MAX_VALUE (TYPE_DOMAIN (type))))
	range = TYPE_MIN_VALUE (TYPE_DOMAIN (type));
      else
	range = build2 (RANGE_EXPR, gfc_array_index_type,
			TYPE_MIN_VALUE (TYPE_DOMAIN (type)),
			TYPE_MAX_VALUE (TYPE_DOMAIN (type)));
      CONSTRUCTOR_APPEND_ELT (v, range, se.expr);
      break;

    case EXPR_ARRAY:
      /* Create a vector of all the elements.  */
      for (c = gfc_constructor_first (expr->value.constructor);
	   c && c->expr; c = gfc_constructor_next (c))
        {
          if (c->iterator)
            {
              /* Problems occur when we get something like
                 integer :: a(lots) = (/(i, i=1, lots)/)  */
              gfc_fatal_error ("The number of elements in the array "
			       "constructor at %L requires an increase of "
			       "the allowed %d upper limit. See "
			       "%<-fmax-array-constructor%> option",
			       &expr->where, flag_max_array_constructor);
	      return NULL_TREE;
	    }
          if (mpz_cmp_si (c->offset, 0) != 0)
            index = gfc_conv_mpz_to_tree (c->offset, gfc_index_integer_kind);
          else
            index = NULL_TREE;

	  if (mpz_cmp_si (c->repeat, 1) > 0)
	    {
	      tree tmp1, tmp2;
	      mpz_t maxval;

	      mpz_init (maxval);
	      mpz_add (maxval, c->offset, c->repeat);
	      mpz_sub_ui (maxval, maxval, 1);
	      tmp2 = gfc_conv_mpz_to_tree (maxval, gfc_index_integer_kind);
	      if (mpz_cmp_si (c->offset, 0) != 0)
		{
		  mpz_add_ui (maxval, c->offset, 1);
		  tmp1 = gfc_conv_mpz_to_tree (maxval, gfc_index_integer_kind);
		}
	      else
		tmp1 = gfc_conv_mpz_to_tree (c->offset, gfc_index_integer_kind);

	      range = fold_build2 (RANGE_EXPR, gfc_array_index_type, tmp1, tmp2);
	      mpz_clear (maxval);
	    }
	  else
	    range = NULL;

          gfc_init_se (&se, NULL);
	  switch (c->expr->expr_type)
	    {
	    case EXPR_CONSTANT:
	      gfc_conv_constant (&se, c->expr);

	      /* See gfortran.dg/charlen_15.f90 for instance.  */
	      if (TREE_CODE (se.expr) == STRING_CST
		  && TREE_CODE (type) == ARRAY_TYPE)
		{
		  tree atype = type;
		  while (TREE_CODE (TREE_TYPE (atype)) == ARRAY_TYPE)
		    atype = TREE_TYPE (atype);
		  gcc_checking_assert (TREE_CODE (TREE_TYPE (atype))
				       == INTEGER_TYPE);
		  gcc_checking_assert (TREE_TYPE (TREE_TYPE (se.expr))
				       == TREE_TYPE (atype));
		  if (tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (se.expr)))
		      > tree_to_uhwi (TYPE_SIZE_UNIT (atype)))
		    {
		      unsigned HOST_WIDE_INT size
			= tree_to_uhwi (TYPE_SIZE_UNIT (atype));
		      const char *p = TREE_STRING_POINTER (se.expr);

		      se.expr = build_string (size, p);
		    }
		  TREE_TYPE (se.expr) = atype;
		}
	      break;

	    case EXPR_STRUCTURE:
              gfc_conv_structure (&se, c->expr, 1);
	      break;

	    default:
	      /* Catch those occasional beasts that do not simplify
		 for one reason or another, assuming that if they are
		 standard defying the frontend will catch them.  */
	      gfc_conv_expr (&se, c->expr);
	      break;
	    }

	  if (range == NULL_TREE)
	    CONSTRUCTOR_APPEND_ELT (v, index, se.expr);
	  else
	    {
	      if (index != NULL_TREE)
		CONSTRUCTOR_APPEND_ELT (v, index, se.expr);
	      CONSTRUCTOR_APPEND_ELT (v, range, se.expr);
	    }
        }
      break;

    case EXPR_NULL:
      return gfc_build_null_descriptor (type);

    default:
      gcc_unreachable ();
    }

  /* Create a constructor from the list of elements.  */
  tmp = build_constructor (type, v);
  TREE_CONSTANT (tmp) = 1;
  return tmp;
}


/* Generate code to evaluate non-constant coarray cobounds.  */

void
gfc_trans_array_cobounds (tree type, stmtblock_t * pblock,
			  const gfc_symbol *sym)
{
  int dim;
  tree ubound;
  tree lbound;
  gfc_se se;
  gfc_array_spec *as;

  as = IS_CLASS_ARRAY (sym) ? CLASS_DATA (sym)->as : sym->as;

  for (dim = as->rank; dim < as->rank + as->corank; dim++)
    {
      /* Evaluate non-constant array bound expressions.
	 F2008 4.5.6.3 para 6: If a specification expression in a scoping unit
	 references a function, the result is finalized before execution of the
	 executable constructs in the scoping unit.
	 Adding the finalblocks enables this.  */
      lbound = GFC_TYPE_ARRAY_LBOUND (type, dim);
      if (as->lower[dim] && !INTEGER_CST_P (lbound))
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_type (&se, as->lower[dim], gfc_array_index_type);
	  gfc_add_block_to_block (pblock, &se.pre);
	  gfc_add_block_to_block (pblock, &se.finalblock);
	  gfc_add_modify (pblock, lbound, se.expr);
	}
      ubound = GFC_TYPE_ARRAY_UBOUND (type, dim);
      if (as->upper[dim] && !INTEGER_CST_P (ubound))
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_type (&se, as->upper[dim], gfc_array_index_type);
	  gfc_add_block_to_block (pblock, &se.pre);
	  gfc_add_block_to_block (pblock, &se.finalblock);
	  gfc_add_modify (pblock, ubound, se.expr);
	}
    }
}


/* Generate code to evaluate non-constant array bounds.  Sets *poffset and
   returns the size (in elements) of the array.  */

tree
gfc_trans_array_bounds (tree type, gfc_symbol * sym, tree * poffset,
                        stmtblock_t * pblock)
{
  gfc_array_spec *as;
  tree size;
  tree stride;
  tree offset;
  tree ubound;
  tree lbound;
  tree tmp;
  gfc_se se;

  int dim;

  as = IS_CLASS_ARRAY (sym) ? CLASS_DATA (sym)->as : sym->as;

  size = gfc_index_one_node;
  offset = gfc_index_zero_node;
  for (dim = 0; dim < as->rank; dim++)
    {
      /* Evaluate non-constant array bound expressions.
	 F2008 4.5.6.3 para 6: If a specification expression in a scoping unit
	 references a function, the result is finalized before execution of the
	 executable constructs in the scoping unit.
	 Adding the finalblocks enables this.  */
      lbound = GFC_TYPE_ARRAY_LBOUND (type, dim);
      if (as->lower[dim] && !INTEGER_CST_P (lbound))
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_type (&se, as->lower[dim], gfc_array_index_type);
	  gfc_add_block_to_block (pblock, &se.pre);
	  gfc_add_block_to_block (pblock, &se.finalblock);
	  gfc_add_modify (pblock, lbound, se.expr);
	}
      ubound = GFC_TYPE_ARRAY_UBOUND (type, dim);
      if (as->upper[dim] && !INTEGER_CST_P (ubound))
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_type (&se, as->upper[dim], gfc_array_index_type);
	  gfc_add_block_to_block (pblock, &se.pre);
	  gfc_add_block_to_block (pblock, &se.finalblock);
	  gfc_add_modify (pblock, ubound, se.expr);
	}
      /* The offset of this dimension.  offset = offset - lbound * stride.  */
      tmp = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			     lbound, size);
      offset = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
				offset, tmp);

      /* The size of this dimension, and the stride of the next.  */
      if (dim + 1 < as->rank)
        stride = GFC_TYPE_ARRAY_STRIDE (type, dim + 1);
      else
	stride = GFC_TYPE_ARRAY_SIZE (type);

      if (ubound != NULL_TREE && !(stride && INTEGER_CST_P (stride)))
	{
	  /* Calculate stride = size * (ubound + 1 - lbound).  */
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type,
				 gfc_index_one_node, lbound);
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, ubound, tmp);
	  tmp = fold_build2_loc (input_location, MULT_EXPR,
				 gfc_array_index_type, size, tmp);
	  if (stride)
	    gfc_add_modify (pblock, stride, tmp);
	  else
	    stride = gfc_evaluate_now (tmp, pblock);

	  /* Make sure that negative size arrays are translated
	     to being zero size.  */
	  tmp = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
				 stride, gfc_index_zero_node);
	  tmp = fold_build3_loc (input_location, COND_EXPR,
				 gfc_array_index_type, tmp,
				 stride, gfc_index_zero_node);
	  gfc_add_modify (pblock, stride, tmp);
	}

      size = stride;
    }

  gfc_trans_array_cobounds (type, pblock, sym);
  gfc_trans_vla_type_sizes (sym, pblock);

  *poffset = offset;
  return size;
}


/* Generate code to initialize/allocate an array variable.  */

void
gfc_trans_auto_array_allocation (tree decl, gfc_symbol * sym,
				 gfc_wrapped_block * block)
{
  stmtblock_t init;
  tree type;
  tree tmp = NULL_TREE;
  tree size;
  tree offset;
  tree space;
  tree inittree;
  bool onstack;

  gcc_assert (!(sym->attr.pointer || sym->attr.allocatable));

  /* Do nothing for USEd variables.  */
  if (sym->attr.use_assoc)
    return;

  type = TREE_TYPE (decl);
  gcc_assert (GFC_ARRAY_TYPE_P (type));
  onstack = TREE_CODE (type) != POINTER_TYPE;

  gfc_init_block (&init);

  /* Evaluate character string length.  */
  if (sym->ts.type == BT_CHARACTER
      && onstack && !INTEGER_CST_P (sym->ts.u.cl->backend_decl))
    {
      gfc_conv_string_length (sym->ts.u.cl, NULL, &init);

      gfc_trans_vla_type_sizes (sym, &init);

      /* Emit a DECL_EXPR for this variable, which will cause the
	 gimplifier to allocate storage, and all that good stuff.  */
      tmp = fold_build1_loc (input_location, DECL_EXPR, TREE_TYPE (decl), decl);
      gfc_add_expr_to_block (&init, tmp);
      if (sym->attr.omp_allocate)
	{
	  /* Save location of size calculation to ensure GOMP_alloc is placed
	     after it.  */
	  tree omp_alloc = lookup_attribute ("omp allocate",
					     DECL_ATTRIBUTES (decl));
	  TREE_CHAIN (TREE_CHAIN (TREE_VALUE (omp_alloc)))
	    = build_tree_list (NULL_TREE, tsi_stmt (tsi_last (init.head)));
	}
    }

  if (onstack)
    {
      gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
      return;
    }

  type = TREE_TYPE (type);

  gcc_assert (!sym->attr.use_assoc);
  gcc_assert (!sym->module);

  if (sym->ts.type == BT_CHARACTER
      && !INTEGER_CST_P (sym->ts.u.cl->backend_decl))
    gfc_conv_string_length (sym->ts.u.cl, NULL, &init);

  size = gfc_trans_array_bounds (type, sym, &offset, &init);

  /* Don't actually allocate space for Cray Pointees.  */
  if (sym->attr.cray_pointee)
    {
      if (VAR_P (GFC_TYPE_ARRAY_OFFSET (type)))
	gfc_add_modify (&init, GFC_TYPE_ARRAY_OFFSET (type), offset);

      gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
      return;
    }
  if (sym->attr.omp_allocate)
    {
      /* The size is the number of elements in the array, so multiply by the
	 size of an element to get the total size.  */
      tmp = TYPE_SIZE_UNIT (gfc_get_element_type (type));
      size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			      size, fold_convert (gfc_array_index_type, tmp));
      size = gfc_evaluate_now (size, &init);

      tree omp_alloc = lookup_attribute ("omp allocate",
					 DECL_ATTRIBUTES (decl));
      TREE_CHAIN (TREE_CHAIN (TREE_VALUE (omp_alloc)))
	= build_tree_list (size, NULL_TREE);
      space = NULL_TREE;
    }
  else if (flag_stack_arrays)
    {
      gcc_assert (TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE);
      space = build_decl (gfc_get_location (&sym->declared_at),
			  VAR_DECL, create_tmp_var_name ("A"),
			  TREE_TYPE (TREE_TYPE (decl)));
      gfc_trans_vla_type_sizes (sym, &init);
    }
  else
    {
      /* The size is the number of elements in the array, so multiply by the
	 size of an element to get the total size.  */
      tmp = TYPE_SIZE_UNIT (gfc_get_element_type (type));
      size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			      size, fold_convert (gfc_array_index_type, tmp));

      /* Allocate memory to hold the data.  */
      tmp = gfc_call_malloc (&init, TREE_TYPE (decl), size);
      gfc_add_modify (&init, decl, tmp);

      /* Free the temporary.  */
      tmp = gfc_call_free (decl);
      space = NULL_TREE;
    }

  /* Set offset of the array.  */
  if (VAR_P (GFC_TYPE_ARRAY_OFFSET (type)))
    gfc_add_modify (&init, GFC_TYPE_ARRAY_OFFSET (type), offset);

  /* Automatic arrays should not have initializers.  */
  gcc_assert (!sym->value);

  inittree = gfc_finish_block (&init);

  if (space)
    {
      tree addr;
      pushdecl (space);

      /* Don't create new scope, emit the DECL_EXPR in exactly the scope
         where also space is located.  */
      gfc_init_block (&init);
      tmp = fold_build1_loc (input_location, DECL_EXPR,
			     TREE_TYPE (space), space);
      gfc_add_expr_to_block (&init, tmp);
      addr = fold_build1_loc (gfc_get_location (&sym->declared_at),
			      ADDR_EXPR, TREE_TYPE (decl), space);
      gfc_add_modify (&init, decl, addr);
      gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
      tmp = NULL_TREE;
    }
  gfc_add_init_cleanup (block, inittree, tmp);
}


/* Generate entry and exit code for g77 calling convention arrays.  */

void
gfc_trans_g77_array (gfc_symbol * sym, gfc_wrapped_block * block)
{
  tree parm;
  tree type;
  locus loc;
  tree offset;
  tree tmp;
  tree stmt;
  stmtblock_t init;

  gfc_save_backend_locus (&loc);
  gfc_set_backend_locus (&sym->declared_at);

  /* Descriptor type.  */
  parm = sym->backend_decl;
  type = TREE_TYPE (parm);
  gcc_assert (GFC_ARRAY_TYPE_P (type));

  gfc_start_block (&init);

  if (sym->ts.type == BT_CHARACTER
      && VAR_P (sym->ts.u.cl->backend_decl))
    gfc_conv_string_length (sym->ts.u.cl, NULL, &init);

  /* Evaluate the bounds of the array.  */
  gfc_trans_array_bounds (type, sym, &offset, &init);

  /* Set the offset.  */
  if (VAR_P (GFC_TYPE_ARRAY_OFFSET (type)))
    gfc_add_modify (&init, GFC_TYPE_ARRAY_OFFSET (type), offset);

  /* Set the pointer itself if we aren't using the parameter directly.  */
  if (TREE_CODE (parm) != PARM_DECL)
    {
      tmp = GFC_DECL_SAVED_DESCRIPTOR (parm);
      if (sym->ts.type == BT_CLASS)
	{
	  tmp = build_fold_indirect_ref_loc (input_location, tmp);
	  tmp = gfc_class_data_get (tmp);
	  tmp = gfc_conv_descriptor_data_get (tmp);
	}
      tmp = convert (TREE_TYPE (parm), tmp);
      gfc_add_modify (&init, parm, tmp);
    }
  stmt = gfc_finish_block (&init);

  gfc_restore_backend_locus (&loc);

  /* Add the initialization code to the start of the function.  */

  if ((sym->ts.type == BT_CLASS && CLASS_DATA (sym)->attr.optional)
      || sym->attr.optional
      || sym->attr.not_always_present)
    {
      tree nullify;
      if (TREE_CODE (parm) != PARM_DECL)
	nullify = fold_build2_loc (input_location, MODIFY_EXPR, void_type_node,
				   parm, null_pointer_node);
      else
	nullify = build_empty_stmt (input_location);
      tmp = gfc_conv_expr_present (sym, true);
      stmt = build3_v (COND_EXPR, tmp, stmt, nullify);
    }

  gfc_add_init_cleanup (block, stmt, NULL_TREE);
}


/* Modify the descriptor of an array parameter so that it has the
   correct lower bound.  Also move the upper bound accordingly.
   If the array is not packed, it will be copied into a temporary.
   For each dimension we set the new lower and upper bounds.  Then we copy the
   stride and calculate the offset for this dimension.  We also work out
   what the stride of a packed array would be, and see it the two match.
   If the array need repacking, we set the stride to the values we just
   calculated, recalculate the offset and copy the array data.
   Code is also added to copy the data back at the end of the function.
   */

void
gfc_trans_dummy_array_bias (gfc_symbol * sym, tree tmpdesc,
			    gfc_wrapped_block * block)
{
  tree size;
  tree type;
  tree offset;
  locus loc;
  stmtblock_t init;
  tree stmtInit, stmtCleanup;
  tree lbound;
  tree ubound;
  tree dubound;
  tree dlbound;
  tree dumdesc;
  tree tmp;
  tree stride, stride2;
  tree stmt_packed;
  tree stmt_unpacked;
  tree partial;
  gfc_se se;
  int n;
  int checkparm;
  int no_repack;
  bool optional_arg;
  gfc_array_spec *as;
  bool is_classarray = IS_CLASS_ARRAY (sym);

  /* Do nothing for pointer and allocatable arrays.  */
  if ((sym->ts.type != BT_CLASS && sym->attr.pointer)
      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)->attr.class_pointer)
      || sym->attr.allocatable
      || (is_classarray && CLASS_DATA (sym)->attr.allocatable))
    return;

  if (!is_classarray && sym->attr.dummy && gfc_is_nodesc_array (sym))
    {
      gfc_trans_g77_array (sym, block);
      return;
    }

  loc.nextc = NULL;
  gfc_save_backend_locus (&loc);
  /* loc.nextc is not set by save_backend_locus but the location routines
     depend on it.  */
  if (loc.nextc == NULL)
    loc.nextc = loc.lb->line;
  gfc_set_backend_locus (&sym->declared_at);

  /* Descriptor type.  */
  type = TREE_TYPE (tmpdesc);
  gcc_assert (GFC_ARRAY_TYPE_P (type));
  dumdesc = GFC_DECL_SAVED_DESCRIPTOR (tmpdesc);
  if (is_classarray)
    /* For a class array the dummy array descriptor is in the _class
       component.  */
    dumdesc = gfc_class_data_get (dumdesc);
  else
    dumdesc = build_fold_indirect_ref_loc (input_location, dumdesc);
  as = IS_CLASS_ARRAY (sym) ? CLASS_DATA (sym)->as : sym->as;
  gfc_start_block (&init);

  if (sym->ts.type == BT_CHARACTER
      && VAR_P (sym->ts.u.cl->backend_decl))
    gfc_conv_string_length (sym->ts.u.cl, NULL, &init);

  /* TODO: Fix the exclusion of class arrays from extent checking.  */
  checkparm = (as->type == AS_EXPLICIT && !is_classarray
	       && (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS));

  no_repack = !(GFC_DECL_PACKED_ARRAY (tmpdesc)
		|| GFC_DECL_PARTIAL_PACKED_ARRAY (tmpdesc));

  if (GFC_DECL_PARTIAL_PACKED_ARRAY (tmpdesc))
    {
      /* For non-constant shape arrays we only check if the first dimension
	 is contiguous.  Repacking higher dimensions wouldn't gain us
	 anything as we still don't know the array stride.  */
      partial = gfc_create_var (logical_type_node, "partial");
      TREE_USED (partial) = 1;
      tmp = gfc_conv_descriptor_stride_get (dumdesc, gfc_rank_cst[0]);
      tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node, tmp,
			     gfc_index_one_node);
      gfc_add_modify (&init, partial, tmp);
    }
  else
    partial = NULL_TREE;

  /* The naming of stmt_unpacked and stmt_packed may be counter-intuitive
     here, however I think it does the right thing.  */
  if (no_repack)
    {
      /* Set the first stride.  */
      stride = gfc_conv_descriptor_stride_get (dumdesc, gfc_rank_cst[0]);
      stride = gfc_evaluate_now (stride, &init);

      tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			     stride, gfc_index_zero_node);
      tmp = fold_build3_loc (input_location, COND_EXPR, gfc_array_index_type,
			     tmp, gfc_index_one_node, stride);
      stride = GFC_TYPE_ARRAY_STRIDE (type, 0);
      gfc_add_modify (&init, stride, tmp);

      /* Allow the user to disable array repacking.  */
      stmt_unpacked = NULL_TREE;
    }
  else
    {
      gcc_assert (integer_onep (GFC_TYPE_ARRAY_STRIDE (type, 0)));
      /* A library call to repack the array if necessary.  */
      tmp = GFC_DECL_SAVED_DESCRIPTOR (tmpdesc);
      stmt_unpacked = build_call_expr_loc (input_location,
				       gfor_fndecl_in_pack, 1, tmp);

      stride = gfc_index_one_node;

      if (warn_array_temporaries)
	gfc_warning (OPT_Warray_temporaries,
		     "Creating array temporary at %L", &loc);
    }

  /* This is for the case where the array data is used directly without
     calling the repack function.  */
  if (no_repack || partial != NULL_TREE)
    stmt_packed = gfc_conv_descriptor_data_get (dumdesc);
  else
    stmt_packed = NULL_TREE;

  /* Assign the data pointer.  */
  if (stmt_packed != NULL_TREE && stmt_unpacked != NULL_TREE)
    {
      /* Don't repack unknown shape arrays when the first stride is 1.  */
      tmp = fold_build3_loc (input_location, COND_EXPR, TREE_TYPE (stmt_packed),
			     partial, stmt_packed, stmt_unpacked);
    }
  else
    tmp = stmt_packed != NULL_TREE ? stmt_packed : stmt_unpacked;
  gfc_add_modify (&init, tmpdesc, fold_convert (type, tmp));

  offset = gfc_index_zero_node;
  size = gfc_index_one_node;

  /* Evaluate the bounds of the array.  */
  for (n = 0; n < as->rank; n++)
    {
      if (checkparm || !as->upper[n])
	{
	  /* Get the bounds of the actual parameter.  */
	  dubound = gfc_conv_descriptor_ubound_get (dumdesc, gfc_rank_cst[n]);
	  dlbound = gfc_conv_descriptor_lbound_get (dumdesc, gfc_rank_cst[n]);
	}
      else
	{
	  dubound = NULL_TREE;
	  dlbound = NULL_TREE;
	}

      lbound = GFC_TYPE_ARRAY_LBOUND (type, n);
      if (!INTEGER_CST_P (lbound))
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_type (&se, as->lower[n],
			      gfc_array_index_type);
	  gfc_add_block_to_block (&init, &se.pre);
	  gfc_add_modify (&init, lbound, se.expr);
	}

      ubound = GFC_TYPE_ARRAY_UBOUND (type, n);
      /* Set the desired upper bound.  */
      if (as->upper[n])
	{
	  /* We know what we want the upper bound to be.  */
	  if (!INTEGER_CST_P (ubound))
	    {
	      gfc_init_se (&se, NULL);
	      gfc_conv_expr_type (&se, as->upper[n],
				  gfc_array_index_type);
	      gfc_add_block_to_block (&init, &se.pre);
	      gfc_add_modify (&init, ubound, se.expr);
	    }

	  /* Check the sizes match.  */
	  if (checkparm)
	    {
	      /* Check (ubound(a) - lbound(a) == ubound(b) - lbound(b)).  */
	      char * msg;
	      tree temp;

	      temp = fold_build2_loc (input_location, MINUS_EXPR,
				      gfc_array_index_type, ubound, lbound);
	      temp = fold_build2_loc (input_location, PLUS_EXPR,
				      gfc_array_index_type,
				      gfc_index_one_node, temp);
	      stride2 = fold_build2_loc (input_location, MINUS_EXPR,
					 gfc_array_index_type, dubound,
					 dlbound);
	      stride2 = fold_build2_loc (input_location, PLUS_EXPR,
					 gfc_array_index_type,
					 gfc_index_one_node, stride2);
	      tmp = fold_build2_loc (input_location, NE_EXPR,
				     gfc_array_index_type, temp, stride2);
	      msg = xasprintf ("Dimension %d of array '%s' has extent "
			       "%%ld instead of %%ld", n+1, sym->name);

	      gfc_trans_runtime_check (true, false, tmp, &init, &loc, msg,
			fold_convert (long_integer_type_node, temp),
			fold_convert (long_integer_type_node, stride2));

	      free (msg);
	    }
	}
      else
	{
	  /* For assumed shape arrays move the upper bound by the same amount
	     as the lower bound.  */
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type, dubound, dlbound);
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, tmp, lbound);
	  gfc_add_modify (&init, ubound, tmp);
	}
      /* The offset of this dimension.  offset = offset - lbound * stride.  */
      tmp = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			     lbound, stride);
      offset = fold_build2_loc (input_location, MINUS_EXPR,
				gfc_array_index_type, offset, tmp);

      /* The size of this dimension, and the stride of the next.  */
      if (n + 1 < as->rank)
	{
	  stride = GFC_TYPE_ARRAY_STRIDE (type, n + 1);

	  if (no_repack || partial != NULL_TREE)
	    stmt_unpacked =
	      gfc_conv_descriptor_stride_get (dumdesc, gfc_rank_cst[n+1]);

	  /* Figure out the stride if not a known constant.  */
	  if (!INTEGER_CST_P (stride))
	    {
	      if (no_repack)
		stmt_packed = NULL_TREE;
	      else
		{
		  /* Calculate stride = size * (ubound + 1 - lbound).  */
		  tmp = fold_build2_loc (input_location, MINUS_EXPR,
					 gfc_array_index_type,
					 gfc_index_one_node, lbound);
		  tmp = fold_build2_loc (input_location, PLUS_EXPR,
					 gfc_array_index_type, ubound, tmp);
		  size = fold_build2_loc (input_location, MULT_EXPR,
					  gfc_array_index_type, size, tmp);
		  stmt_packed = size;
		}

	      /* Assign the stride.  */
	      if (stmt_packed != NULL_TREE && stmt_unpacked != NULL_TREE)
		tmp = fold_build3_loc (input_location, COND_EXPR,
				       gfc_array_index_type, partial,
				       stmt_unpacked, stmt_packed);
	      else
		tmp = (stmt_packed != NULL_TREE) ? stmt_packed : stmt_unpacked;
	      gfc_add_modify (&init, stride, tmp);
	    }
	}
      else
	{
	  stride = GFC_TYPE_ARRAY_SIZE (type);

	  if (stride && !INTEGER_CST_P (stride))
	    {
	      /* Calculate size = stride * (ubound + 1 - lbound).  */
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     gfc_array_index_type,
				     gfc_index_one_node, lbound);
	      tmp = fold_build2_loc (input_location, PLUS_EXPR,
				     gfc_array_index_type,
				     ubound, tmp);
	      tmp = fold_build2_loc (input_location, MULT_EXPR,
				     gfc_array_index_type,
				     GFC_TYPE_ARRAY_STRIDE (type, n), tmp);
	      gfc_add_modify (&init, stride, tmp);
	    }
	}
    }

  gfc_trans_array_cobounds (type, &init, sym);

  /* Set the offset.  */
  if (VAR_P (GFC_TYPE_ARRAY_OFFSET (type)))
    gfc_add_modify (&init, GFC_TYPE_ARRAY_OFFSET (type), offset);

  gfc_trans_vla_type_sizes (sym, &init);

  stmtInit = gfc_finish_block (&init);

  /* Only do the entry/initialization code if the arg is present.  */
  dumdesc = GFC_DECL_SAVED_DESCRIPTOR (tmpdesc);
  optional_arg = (sym->attr.optional
		  || (sym->ns->proc_name->attr.entry_master
		      && sym->attr.dummy));
  if (optional_arg)
    {
      tree zero_init = fold_convert (TREE_TYPE (tmpdesc), null_pointer_node);
      zero_init = fold_build2_loc (input_location, MODIFY_EXPR, void_type_node,
				   tmpdesc, zero_init);
      tmp = gfc_conv_expr_present (sym, true);
      stmtInit = build3_v (COND_EXPR, tmp, stmtInit, zero_init);
    }

  /* Cleanup code.  */
  if (no_repack)
    stmtCleanup = NULL_TREE;
  else
    {
      stmtblock_t cleanup;
      gfc_start_block (&cleanup);

      if (sym->attr.intent != INTENT_IN)
	{
	  /* Copy the data back.  */
	  tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_in_unpack, 2, dumdesc, tmpdesc);
	  gfc_add_expr_to_block (&cleanup, tmp);
	}

      /* Free the temporary.  */
      tmp = gfc_call_free (tmpdesc);
      gfc_add_expr_to_block (&cleanup, tmp);

      stmtCleanup = gfc_finish_block (&cleanup);

      /* Only do the cleanup if the array was repacked.  */
      if (is_classarray)
	/* For a class array the dummy array descriptor is in the _class
	   component.  */
	tmp = gfc_class_data_get (dumdesc);
      else
	tmp = build_fold_indirect_ref_loc (input_location, dumdesc);
      tmp = gfc_conv_descriptor_data_get (tmp);
      tmp = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			     tmp, tmpdesc);
      stmtCleanup = build3_v (COND_EXPR, tmp, stmtCleanup,
			      build_empty_stmt (input_location));

      if (optional_arg)
	{
	  tmp = gfc_conv_expr_present (sym);
	  stmtCleanup = build3_v (COND_EXPR, tmp, stmtCleanup,
				  build_empty_stmt (input_location));
	}
    }

  /* We don't need to free any memory allocated by internal_pack as it will
     be freed at the end of the function by pop_context.  */
  gfc_add_init_cleanup (block, stmtInit, stmtCleanup);

  gfc_restore_backend_locus (&loc);
}


/* Calculate the overall offset, including subreferences.  */
void
gfc_get_dataptr_offset (stmtblock_t *block, tree parm, tree desc, tree offset,
			bool subref, gfc_expr *expr)
{
  tree tmp;
  tree field;
  tree stride;
  tree index;
  gfc_ref *ref;
  gfc_se start;
  int n;

  /* If offset is NULL and this is not a subreferenced array, there is
     nothing to do.  */
  if (offset == NULL_TREE)
    {
      if (subref)
	offset = gfc_index_zero_node;
      else
	return;
    }

  tmp = build_array_ref (desc, offset, NULL, NULL);

  /* Offset the data pointer for pointer assignments from arrays with
     subreferences; e.g. my_integer => my_type(:)%integer_component.  */
  if (subref)
    {
      /* Go past the array reference.  */
      for (ref = expr->ref; ref; ref = ref->next)
	if (ref->type == REF_ARRAY &&
	      ref->u.ar.type != AR_ELEMENT)
	  {
	    ref = ref->next;
	    break;
	  }

      /* Calculate the offset for each subsequent subreference.  */
      for (; ref; ref = ref->next)
	{
	  switch (ref->type)
	    {
	    case REF_COMPONENT:
	      field = ref->u.c.component->backend_decl;
	      gcc_assert (field && TREE_CODE (field) == FIELD_DECL);
	      tmp = fold_build3_loc (input_location, COMPONENT_REF,
				     TREE_TYPE (field),
				     tmp, field, NULL_TREE);
	      break;

	    case REF_SUBSTRING:
	      gcc_assert (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE);
	      gfc_init_se (&start, NULL);
	      gfc_conv_expr_type (&start, ref->u.ss.start, gfc_charlen_type_node);
	      gfc_add_block_to_block (block, &start.pre);
	      tmp = gfc_build_array_ref (tmp, start.expr, NULL);
	      break;

	    case REF_ARRAY:
	      gcc_assert (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE
			    && ref->u.ar.type == AR_ELEMENT);

	      /* TODO - Add bounds checking.  */
	      stride = gfc_index_one_node;
	      index = gfc_index_zero_node;
	      for (n = 0; n < ref->u.ar.dimen; n++)
		{
		  tree itmp;
		  tree jtmp;

		  /* Update the index.  */
		  gfc_init_se (&start, NULL);
		  gfc_conv_expr_type (&start, ref->u.ar.start[n], gfc_array_index_type);
		  itmp = gfc_evaluate_now (start.expr, block);
		  gfc_init_se (&start, NULL);
		  gfc_conv_expr_type (&start, ref->u.ar.as->lower[n], gfc_array_index_type);
		  jtmp = gfc_evaluate_now (start.expr, block);
		  itmp = fold_build2_loc (input_location, MINUS_EXPR,
					  gfc_array_index_type, itmp, jtmp);
		  itmp = fold_build2_loc (input_location, MULT_EXPR,
					  gfc_array_index_type, itmp, stride);
		  index = fold_build2_loc (input_location, PLUS_EXPR,
					  gfc_array_index_type, itmp, index);
		  index = gfc_evaluate_now (index, block);

		  /* Update the stride.  */
		  gfc_init_se (&start, NULL);
		  gfc_conv_expr_type (&start, ref->u.ar.as->upper[n], gfc_array_index_type);
		  itmp =  fold_build2_loc (input_location, MINUS_EXPR,
					   gfc_array_index_type, start.expr,
					   jtmp);
		  itmp =  fold_build2_loc (input_location, PLUS_EXPR,
					   gfc_array_index_type,
					   gfc_index_one_node, itmp);
		  stride =  fold_build2_loc (input_location, MULT_EXPR,
					     gfc_array_index_type, stride, itmp);
		  stride = gfc_evaluate_now (stride, block);
		}

	      /* Apply the index to obtain the array element.  */
	      tmp = gfc_build_array_ref (tmp, index, NULL);
	      break;

	    case REF_INQUIRY:
	      switch (ref->u.i)
		{
		case INQUIRY_RE:
		  tmp = fold_build1_loc (input_location, REALPART_EXPR,
					 TREE_TYPE (TREE_TYPE (tmp)), tmp);
		  break;

		case INQUIRY_IM:
		  tmp = fold_build1_loc (input_location, IMAGPART_EXPR,
					 TREE_TYPE (TREE_TYPE (tmp)), tmp);
		  break;

		default:
		  break;
		}
	      break;

	    default:
	      gcc_unreachable ();
	      break;
	    }
	}
    }

  /* Set the target data pointer.  */
  offset = gfc_build_addr_expr (gfc_array_dataptr_type (desc), tmp);

  /* Check for optional dummy argument being present.  Arguments of BIND(C)
     procedures are excepted here since they are handled differently.  */
  if (expr->expr_type == EXPR_VARIABLE
      && expr->symtree->n.sym->attr.dummy
      && expr->symtree->n.sym->attr.optional
      && !is_CFI_desc (NULL, expr))
    offset = build3_loc (input_location, COND_EXPR, TREE_TYPE (offset),
			 gfc_conv_expr_present (expr->symtree->n.sym), offset,
			 fold_convert (TREE_TYPE (offset), gfc_index_zero_node));

  gfc_conv_descriptor_data_set (block, parm, offset);
}


/* gfc_conv_expr_descriptor needs the string length an expression
   so that the size of the temporary can be obtained.  This is done
   by adding up the string lengths of all the elements in the
   expression.  Function with non-constant expressions have their
   string lengths mapped onto the actual arguments using the
   interface mapping machinery in trans-expr.cc.  */
static void
get_array_charlen (gfc_expr *expr, gfc_se *se)
{
  gfc_interface_mapping mapping;
  gfc_formal_arglist *formal;
  gfc_actual_arglist *arg;
  gfc_se tse;
  gfc_expr *e;

  if (expr->ts.u.cl->length
	&& gfc_is_constant_expr (expr->ts.u.cl->length))
    {
      if (!expr->ts.u.cl->backend_decl)
	gfc_conv_string_length (expr->ts.u.cl, expr, &se->pre);
      return;
    }

  switch (expr->expr_type)
    {
    case EXPR_ARRAY:

      /* This is somewhat brutal. The expression for the first
	 element of the array is evaluated and assigned to a
	 new string length for the original expression.  */
      e = gfc_constructor_first (expr->value.constructor)->expr;

      gfc_init_se (&tse, NULL);

      /* Avoid evaluating trailing array references since all we need is
	 the string length.  */
      if (e->rank)
	tse.descriptor_only = 1;
      if (e->rank && e->expr_type != EXPR_VARIABLE)
	gfc_conv_expr_descriptor (&tse, e);
      else
	gfc_conv_expr (&tse, e);

      gfc_add_block_to_block (&se->pre, &tse.pre);
      gfc_add_block_to_block (&se->post, &tse.post);

      if (!expr->ts.u.cl->backend_decl || !VAR_P (expr->ts.u.cl->backend_decl))
	{
	  expr->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
	  expr->ts.u.cl->backend_decl =
			gfc_create_var (gfc_charlen_type_node, "sln");
	}

      gfc_add_modify (&se->pre, expr->ts.u.cl->backend_decl,
		      tse.string_length);

      /* Make sure that deferred length components point to the hidden
	 string_length component.  */
      if (TREE_CODE (tse.expr) == COMPONENT_REF
	  && TREE_CODE (tse.string_length) == COMPONENT_REF
	  && TREE_OPERAND (tse.expr, 0) == TREE_OPERAND (tse.string_length, 0))
	e->ts.u.cl->backend_decl = expr->ts.u.cl->backend_decl;

      return;

    case EXPR_OP:
      get_array_charlen (expr->value.op.op1, se);

      /* For parentheses the expression ts.u.cl should be identical.  */
      if (expr->value.op.op == INTRINSIC_PARENTHESES)
	{
	  if (expr->value.op.op1->ts.u.cl != expr->ts.u.cl)
	    expr->ts.u.cl->backend_decl
			= expr->value.op.op1->ts.u.cl->backend_decl;
	  return;
	}

      expr->ts.u.cl->backend_decl =
		gfc_create_var (gfc_charlen_type_node, "sln");

      if (expr->value.op.op2)
	{
	  get_array_charlen (expr->value.op.op2, se);

	  gcc_assert (expr->value.op.op == INTRINSIC_CONCAT);

	  /* Add the string lengths and assign them to the expression
	     string length backend declaration.  */
	  gfc_add_modify (&se->pre, expr->ts.u.cl->backend_decl,
			  fold_build2_loc (input_location, PLUS_EXPR,
				gfc_charlen_type_node,
				expr->value.op.op1->ts.u.cl->backend_decl,
				expr->value.op.op2->ts.u.cl->backend_decl));
	}
      else
	gfc_add_modify (&se->pre, expr->ts.u.cl->backend_decl,
			expr->value.op.op1->ts.u.cl->backend_decl);
      break;

    case EXPR_FUNCTION:
      if (expr->value.function.esym == NULL
	    || expr->ts.u.cl->length->expr_type == EXPR_CONSTANT)
	{
	  gfc_conv_string_length (expr->ts.u.cl, expr, &se->pre);
	  break;
	}

      /* Map expressions involving the dummy arguments onto the actual
	 argument expressions.  */
      gfc_init_interface_mapping (&mapping);
      formal = gfc_sym_get_dummy_args (expr->symtree->n.sym);
      arg = expr->value.function.actual;

      /* Set se = NULL in the calls to the interface mapping, to suppress any
	 backend stuff.  */
      for (; arg != NULL; arg = arg->next, formal = formal ? formal->next : NULL)
	{
	  if (!arg->expr)
	    continue;
	  if (formal->sym)
	  gfc_add_interface_mapping (&mapping, formal->sym, NULL, arg->expr);
	}

      gfc_init_se (&tse, NULL);

      /* Build the expression for the character length and convert it.  */
      gfc_apply_interface_mapping (&mapping, &tse, expr->ts.u.cl->length);

      gfc_add_block_to_block (&se->pre, &tse.pre);
      gfc_add_block_to_block (&se->post, &tse.post);
      tse.expr = fold_convert (gfc_charlen_type_node, tse.expr);
      tse.expr = fold_build2_loc (input_location, MAX_EXPR,
				  TREE_TYPE (tse.expr), tse.expr,
				  build_zero_cst (TREE_TYPE (tse.expr)));
      expr->ts.u.cl->backend_decl = tse.expr;
      gfc_free_interface_mapping (&mapping);
      break;

    default:
      gfc_conv_string_length (expr->ts.u.cl, expr, &se->pre);
      break;
    }
}


/* Helper function to check dimensions.  */
static bool
transposed_dims (gfc_ss *ss)
{
  int n;

  for (n = 0; n < ss->dimen; n++)
    if (ss->dim[n] != n)
      return true;
  return false;
}


/* Convert the last ref of a scalar coarray from an AR_ELEMENT to an
   AR_FULL, suitable for the scalarizer.  */

static gfc_ss *
walk_coarray (gfc_expr *e)
{
  gfc_ss *ss;

  gcc_assert (gfc_get_corank (e) > 0);

  ss = gfc_walk_expr (e);

  /* Fix scalar coarray.  */
  if (ss == gfc_ss_terminator)
    {
      gfc_ref *ref;

      ref = e->ref;
      while (ref)
	{
	  if (ref->type == REF_ARRAY
	      && ref->u.ar.codimen > 0)
	    break;

	  ref = ref->next;
	}

      gcc_assert (ref != NULL);
      if (ref->u.ar.type == AR_ELEMENT)
	ref->u.ar.type = AR_SECTION;
      ss = gfc_reverse_ss (gfc_walk_array_ref (ss, e, ref));
    }

  return ss;
}


/* Convert an array for passing as an actual argument.  Expressions and
   vector subscripts are evaluated and stored in a temporary, which is then
   passed.  For whole arrays the descriptor is passed.  For array sections
   a modified copy of the descriptor is passed, but using the original data.

   This function is also used for array pointer assignments, and there
   are three cases:

     - se->want_pointer && !se->direct_byref
	 EXPR is an actual argument.  On exit, se->expr contains a
	 pointer to the array descriptor.

     - !se->want_pointer && !se->direct_byref
	 EXPR is an actual argument to an intrinsic function or the
	 left-hand side of a pointer assignment.  On exit, se->expr
	 contains the descriptor for EXPR.

     - !se->want_pointer && se->direct_byref
	 EXPR is the right-hand side of a pointer assignment and
	 se->expr is the descriptor for the previously-evaluated
	 left-hand side.  The function creates an assignment from
	 EXPR to se->expr.


   The se->force_tmp flag disables the non-copying descriptor optimization
   that is used for transpose. It may be used in cases where there is an
   alias between the transpose argument and another argument in the same
   function call.  */

void
gfc_conv_expr_descriptor (gfc_se *se, gfc_expr *expr)
{
  gfc_ss *ss;
  gfc_ss_type ss_type;
  gfc_ss_info *ss_info;
  gfc_loopinfo loop;
  gfc_array_info *info;
  int need_tmp;
  int n;
  tree tmp;
  tree desc;
  stmtblock_t block;
  tree start;
  int full;
  bool subref_array_target = false;
  bool deferred_array_component = false;
  bool substr = false;
  gfc_expr *arg, *ss_expr;

  if (se->want_coarray)
    ss = walk_coarray (expr);
  else
    ss = gfc_walk_expr (expr);

  gcc_assert (ss != NULL);
  gcc_assert (ss != gfc_ss_terminator);

  ss_info = ss->info;
  ss_type = ss_info->type;
  ss_expr = ss_info->expr;

  /* Special case: TRANSPOSE which needs no temporary.  */
  while (expr->expr_type == EXPR_FUNCTION && expr->value.function.isym
	 && (arg = gfc_get_noncopying_intrinsic_argument (expr)) != NULL)
    {
      /* This is a call to transpose which has already been handled by the
	 scalarizer, so that we just need to get its argument's descriptor.  */
      gcc_assert (expr->value.function.isym->id == GFC_ISYM_TRANSPOSE);
      expr = expr->value.function.actual->expr;
    }

  if (!se->direct_byref)
    se->unlimited_polymorphic = UNLIMITED_POLY (expr);

  /* Special case things we know we can pass easily.  */
  switch (expr->expr_type)
    {
    case EXPR_VARIABLE:
      /* If we have a linear array section, we can pass it directly.
	 Otherwise we need to copy it into a temporary.  */

      gcc_assert (ss_type == GFC_SS_SECTION);
      gcc_assert (ss_expr == expr);
      info = &ss_info->data.array;

      /* Get the descriptor for the array.  */
      gfc_conv_ss_descriptor (&se->pre, ss, 0);
      desc = info->descriptor;

      /* The charlen backend decl for deferred character components cannot
	 be used because it is fixed at zero.  Instead, the hidden string
	 length component is used.  */
      if (expr->ts.type == BT_CHARACTER
	  && expr->ts.deferred
	  && TREE_CODE (desc) == COMPONENT_REF)
	deferred_array_component = true;

      substr = info->ref && info->ref->next
	       && info->ref->next->type == REF_SUBSTRING;

      subref_array_target = (is_subref_array (expr)
			     && (se->direct_byref
				 || expr->ts.type == BT_CHARACTER));
      need_tmp = (gfc_ref_needs_temporary_p (expr->ref)
		  && !subref_array_target);

      if (se->force_tmp)
	need_tmp = 1;
      else if (se->force_no_tmp)
	need_tmp = 0;

      if (need_tmp)
	full = 0;
      else if (GFC_ARRAY_TYPE_P (TREE_TYPE (desc)))
	{
	  /* Create a new descriptor if the array doesn't have one.  */
	  full = 0;
	}
      else if (info->ref->u.ar.type == AR_FULL || se->descriptor_only)
	full = 1;
      else if (se->direct_byref)
	full = 0;
      else if (info->ref->u.ar.dimen == 0 && !info->ref->next)
	full = 1;
      else if (info->ref->u.ar.type == AR_SECTION && se->want_pointer)
	full = 0;
      else
	full = gfc_full_array_ref_p (info->ref, NULL);

      if (full && !transposed_dims (ss))
	{
	  if (se->direct_byref && !se->byref_noassign)
	    {
	      /* Copy the descriptor for pointer assignments.  */
	      gfc_add_modify (&se->pre, se->expr, desc);

	      /* Add any offsets from subreferences.  */
	      gfc_get_dataptr_offset (&se->pre, se->expr, desc, NULL_TREE,
				      subref_array_target, expr);

	      /* ....and set the span field.  */
	      if (ss_info->expr->ts.type == BT_CHARACTER)
		tmp = gfc_conv_descriptor_span_get (desc);
	      else
		tmp = gfc_get_array_span (desc, expr);
	      gfc_conv_descriptor_span_set (&se->pre, se->expr, tmp);
	    }
	  else if (se->want_pointer)
	    {
	      /* We pass full arrays directly.  This means that pointers and
		 allocatable arrays should also work.  */
	      se->expr = gfc_build_addr_expr (NULL_TREE, desc);
	    }
	  else
	    {
	      se->expr = desc;
	    }

	  if (expr->ts.type == BT_CHARACTER && !deferred_array_component)
	    se->string_length = gfc_get_expr_charlen (expr);
	  /* The ss_info string length is returned set to the value of the
	     hidden string length component.  */
	  else if (deferred_array_component)
	    se->string_length = ss_info->string_length;

	  se->class_container = ss_info->class_container;

	  gfc_free_ss_chain (ss);
	  return;
	}
      break;

    case EXPR_FUNCTION:
      /* A transformational function return value will be a temporary
	 array descriptor.  We still need to go through the scalarizer
	 to create the descriptor.  Elemental functions are handled as
	 arbitrary expressions, i.e. copy to a temporary.  */

      if (se->direct_byref)
	{
	  gcc_assert (ss_type == GFC_SS_FUNCTION && ss_expr == expr);

	  /* For pointer assignments pass the descriptor directly.  */
	  if (se->ss == NULL)
	    se->ss = ss;
	  else
	    gcc_assert (se->ss == ss);

	  if (!is_pointer_array (se->expr))
	    {
	      tmp = gfc_get_element_type (TREE_TYPE (se->expr));
	      tmp = fold_convert (gfc_array_index_type,
				  size_in_bytes (tmp));
	      gfc_conv_descriptor_span_set (&se->pre, se->expr, tmp);
	    }

	  se->expr = gfc_build_addr_expr (NULL_TREE, se->expr);
	  gfc_conv_expr (se, expr);

	  gfc_free_ss_chain (ss);
	  return;
	}

      if (ss_expr != expr || ss_type != GFC_SS_FUNCTION)
	{
	  if (ss_expr != expr)
	    /* Elemental function.  */
	    gcc_assert ((expr->value.function.esym != NULL
			 && expr->value.function.esym->attr.elemental)
			|| (expr->value.function.isym != NULL
			    && expr->value.function.isym->elemental)
			|| (gfc_expr_attr (expr).proc_pointer
			    && gfc_expr_attr (expr).elemental)
			|| gfc_inline_intrinsic_function_p (expr));

	  need_tmp = 1;
	  if (expr->ts.type == BT_CHARACTER
		&& expr->ts.u.cl->length
		&& expr->ts.u.cl->length->expr_type != EXPR_CONSTANT)
	    get_array_charlen (expr, se);

	  info = NULL;
	}
      else
	{
	  /* Transformational function.  */
	  info = &ss_info->data.array;
	  need_tmp = 0;
	}
      break;

    case EXPR_ARRAY:
      /* Constant array constructors don't need a temporary.  */
      if (ss_type == GFC_SS_CONSTRUCTOR
	  && expr->ts.type != BT_CHARACTER
	  && gfc_constant_array_constructor_p (expr->value.constructor))
	{
	  need_tmp = 0;
	  info = &ss_info->data.array;
	}
      else
	{
	  need_tmp = 1;
	  info = NULL;
	}
      break;

    default:
      /* Something complicated.  Copy it into a temporary.  */
      need_tmp = 1;
      info = NULL;
      break;
    }

  /* If we are creating a temporary, we don't need to bother about aliases
     anymore.  */
  if (need_tmp)
    se->force_tmp = 0;

  gfc_init_loopinfo (&loop);

  /* Associate the SS with the loop.  */
  gfc_add_ss_to_loop (&loop, ss);

  /* Tell the scalarizer not to bother creating loop variables, etc.  */
  if (!need_tmp)
    loop.array_parameter = 1;
  else
    /* The right-hand side of a pointer assignment mustn't use a temporary.  */
    gcc_assert (!se->direct_byref);

  /* Do we need bounds checking or not?  */
  ss->no_bounds_check = expr->no_bounds_check;

  /* Setup the scalarizing loops and bounds.  */
  gfc_conv_ss_startstride (&loop);

  /* Add bounds-checking for elemental dimensions.  */
  if ((gfc_option.rtcheck & GFC_RTCHECK_BOUNDS) && !expr->no_bounds_check)
    array_bound_check_elemental (se, ss, expr);

  if (need_tmp)
    {
      if (expr->ts.type == BT_CHARACTER
	  && (!expr->ts.u.cl->backend_decl || expr->expr_type == EXPR_ARRAY))
	get_array_charlen (expr, se);

      /* Tell the scalarizer to make a temporary.  */
      loop.temp_ss = gfc_get_temp_ss (gfc_typenode_for_spec (&expr->ts),
				      ((expr->ts.type == BT_CHARACTER)
				       ? expr->ts.u.cl->backend_decl
				       : NULL),
				      loop.dimen);

      se->string_length = loop.temp_ss->info->string_length;
      gcc_assert (loop.temp_ss->dimen == loop.dimen);
      gfc_add_ss_to_loop (&loop, loop.temp_ss);
    }

  gfc_conv_loop_setup (&loop, & expr->where);

  if (need_tmp)
    {
      /* Copy into a temporary and pass that.  We don't need to copy the data
         back because expressions and vector subscripts must be INTENT_IN.  */
      /* TODO: Optimize passing function return values.  */
      gfc_se lse;
      gfc_se rse;
      bool deep_copy;

      /* Start the copying loops.  */
      gfc_mark_ss_chain_used (loop.temp_ss, 1);
      gfc_mark_ss_chain_used (ss, 1);
      gfc_start_scalarized_body (&loop, &block);

      /* Copy each data element.  */
      gfc_init_se (&lse, NULL);
      gfc_copy_loopinfo_to_se (&lse, &loop);
      gfc_init_se (&rse, NULL);
      gfc_copy_loopinfo_to_se (&rse, &loop);

      lse.ss = loop.temp_ss;
      rse.ss = ss;

      gfc_conv_tmp_array_ref (&lse);
      if (expr->ts.type == BT_CHARACTER)
	{
	  gfc_conv_expr (&rse, expr);
	  if (POINTER_TYPE_P (TREE_TYPE (rse.expr)))
	    rse.expr = build_fold_indirect_ref_loc (input_location,
						rse.expr);
	}
      else
        gfc_conv_expr_val (&rse, expr);

      gfc_add_block_to_block (&block, &rse.pre);
      gfc_add_block_to_block (&block, &lse.pre);

      lse.string_length = rse.string_length;

      deep_copy = !se->data_not_needed
		  && (expr->expr_type == EXPR_VARIABLE
		      || expr->expr_type == EXPR_ARRAY);
      tmp = gfc_trans_scalar_assign (&lse, &rse, expr->ts,
				     deep_copy, false);
      gfc_add_expr_to_block (&block, tmp);

      /* Finish the copying loops.  */
      gfc_trans_scalarizing_loops (&loop, &block);

      desc = loop.temp_ss->info->data.array.descriptor;
    }
  else if (expr->expr_type == EXPR_FUNCTION && !transposed_dims (ss))
    {
      desc = info->descriptor;
      se->string_length = ss_info->string_length;
    }
  else
    {
      /* We pass sections without copying to a temporary.  Make a new
	 descriptor and point it at the section we want.  The loop variable
	 limits will be the limits of the section.
	 A function may decide to repack the array to speed up access, but
	 we're not bothered about that here.  */
      int dim, ndim, codim;
      tree parm;
      tree parmtype;
      tree dtype;
      tree stride;
      tree from;
      tree to;
      tree base;
      tree offset;

      ndim = info->ref ? info->ref->u.ar.dimen : ss->dimen;

      if (se->want_coarray)
	{
	  gfc_array_ref *ar = &info->ref->u.ar;

	  codim = gfc_get_corank (expr);
	  for (n = 0; n < codim - 1; n++)
	    {
	      /* Make sure we are not lost somehow.  */
	      gcc_assert (ar->dimen_type[n + ndim] == DIMEN_THIS_IMAGE);

	      /* Make sure the call to gfc_conv_section_startstride won't
		 generate unnecessary code to calculate stride.  */
	      gcc_assert (ar->stride[n + ndim] == NULL);

	      gfc_conv_section_startstride (&loop.pre, ss, n + ndim);
	      loop.from[n + loop.dimen] = info->start[n + ndim];
	      loop.to[n + loop.dimen]   = info->end[n + ndim];
	    }

	  gcc_assert (n == codim - 1);
	  evaluate_bound (&loop.pre, info->start, ar->start,
			  info->descriptor, n + ndim, true,
			  ar->as->type == AS_DEFERRED);
	  loop.from[n + loop.dimen] = info->start[n + ndim];
	}
      else
	codim = 0;

      /* Set the string_length for a character array.  */
      if (expr->ts.type == BT_CHARACTER)
	{
	  if (deferred_array_component && !substr)
	    se->string_length = ss_info->string_length;
	  else
	    se->string_length =  gfc_get_expr_charlen (expr);

	  if (VAR_P (se->string_length)
	      && expr->ts.u.cl->backend_decl == se->string_length)
	    tmp = ss_info->string_length;
	  else
	    tmp = se->string_length;

	  if (expr->ts.deferred && expr->ts.u.cl->backend_decl
	      && VAR_P (expr->ts.u.cl->backend_decl))
	    gfc_add_modify (&se->pre, expr->ts.u.cl->backend_decl, tmp);
	  else
	    expr->ts.u.cl->backend_decl = tmp;
	}

      /* If we have an array section, are assigning  or passing an array
	 section argument make sure that the lower bound is 1.  References
	 to the full array should otherwise keep the original bounds.  */
      if (!info->ref || info->ref->u.ar.type != AR_FULL)
	for (dim = 0; dim < loop.dimen; dim++)
	  if (!integer_onep (loop.from[dim]))
	    {
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     gfc_array_index_type, gfc_index_one_node,
				     loop.from[dim]);
	      loop.to[dim] = fold_build2_loc (input_location, PLUS_EXPR,
					      gfc_array_index_type,
					      loop.to[dim], tmp);
	      loop.from[dim] = gfc_index_one_node;
	    }

      desc = info->descriptor;
      if (se->direct_byref && !se->byref_noassign)
	{
	  /* For pointer assignments we fill in the destination.  */
	  parm = se->expr;
	  parmtype = TREE_TYPE (parm);
	}
      else
	{
	  /* Otherwise make a new one.  */
	  if (expr->ts.type == BT_CHARACTER)
	    parmtype = gfc_typenode_for_spec (&expr->ts);
	  else
	    parmtype = gfc_get_element_type (TREE_TYPE (desc));

	  parmtype = gfc_get_array_type_bounds (parmtype, loop.dimen, codim,
						loop.from, loop.to, 0,
						GFC_ARRAY_UNKNOWN, false);
	  parm = gfc_create_var (parmtype, "parm");

	  /* When expression is a class object, then add the class' handle to
	     the parm_decl.  */
	  if (expr->ts.type == BT_CLASS && expr->expr_type == EXPR_VARIABLE)
	    {
	      gfc_expr *class_expr = gfc_find_and_cut_at_last_class_ref (expr);
	      gfc_se classse;

	      /* class_expr can be NULL, when no _class ref is in expr.
		 We must not fix this here with a gfc_fix_class_ref ().  */
	      if (class_expr)
		{
		  gfc_init_se (&classse, NULL);
		  gfc_conv_expr (&classse, class_expr);
		  gfc_free_expr (class_expr);

		  gcc_assert (classse.pre.head == NULL_TREE
			      && classse.post.head == NULL_TREE);
		  gfc_allocate_lang_decl (parm);
		  GFC_DECL_SAVED_DESCRIPTOR (parm) = classse.expr;
		}
	    }
	}

      if (expr->ts.type == BT_CHARACTER
	  && VAR_P (TYPE_SIZE_UNIT (gfc_get_element_type (TREE_TYPE (parm)))))
	{
	  tree elem_len = TYPE_SIZE_UNIT (gfc_get_element_type (TREE_TYPE (parm)));
	  gfc_add_modify (&loop.pre, elem_len,
			  fold_convert (TREE_TYPE (elem_len),
			  gfc_get_array_span (desc, expr)));
	}

      /* Set the span field.  */
      tmp = NULL_TREE;
      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))
	tmp = gfc_conv_descriptor_span_get (desc);
      else
	tmp = gfc_get_array_span (desc, expr);
      if (tmp)
	gfc_conv_descriptor_span_set (&loop.pre, parm, tmp);

      /* The following can be somewhat confusing.  We have two
         descriptors, a new one and the original array.
         {parm, parmtype, dim} refer to the new one.
         {desc, type, n, loop} refer to the original, which maybe
         a descriptorless array.
         The bounds of the scalarization are the bounds of the section.
         We don't have to worry about numeric overflows when calculating
         the offsets because all elements are within the array data.  */

      /* Set the dtype.  */
      tmp = gfc_conv_descriptor_dtype (parm);
      if (se->unlimited_polymorphic)
	dtype = gfc_get_dtype (TREE_TYPE (desc), &loop.dimen);
      else if (expr->ts.type == BT_ASSUMED)
	{
	  tree tmp2 = desc;
	  if (DECL_LANG_SPECIFIC (tmp2) && GFC_DECL_SAVED_DESCRIPTOR (tmp2))
	    tmp2 = GFC_DECL_SAVED_DESCRIPTOR (tmp2);
	  if (POINTER_TYPE_P (TREE_TYPE (tmp2)))
	    tmp2 = build_fold_indirect_ref_loc (input_location, tmp2);
	  dtype = gfc_conv_descriptor_dtype (tmp2);
	}
      else
	dtype = gfc_get_dtype (parmtype);
      gfc_add_modify (&loop.pre, tmp, dtype);

      /* The 1st element in the section.  */
      base = gfc_index_zero_node;

      /* The offset from the 1st element in the section.  */
      offset = gfc_index_zero_node;

      for (n = 0; n < ndim; n++)
	{
	  stride = gfc_conv_array_stride (desc, n);

	  /* Work out the 1st element in the section.  */
	  if (info->ref
	      && info->ref->u.ar.dimen_type[n] == DIMEN_ELEMENT)
	    {
	      gcc_assert (info->subscript[n]
			  && info->subscript[n]->info->type == GFC_SS_SCALAR);
	      start = info->subscript[n]->info->data.scalar.value;
	    }
	  else
	    {
	      /* Evaluate and remember the start of the section.  */
	      start = info->start[n];
	      stride = gfc_evaluate_now (stride, &loop.pre);
	    }

	  tmp = gfc_conv_array_lbound (desc, n);
	  tmp = fold_build2_loc (input_location, MINUS_EXPR, TREE_TYPE (tmp),
				 start, tmp);
	  tmp = fold_build2_loc (input_location, MULT_EXPR, TREE_TYPE (tmp),
				 tmp, stride);
	  base = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (tmp),
				    base, tmp);

	  if (info->ref
	      && info->ref->u.ar.dimen_type[n] == DIMEN_ELEMENT)
	    {
	      /* For elemental dimensions, we only need the 1st
		 element in the section.  */
	      continue;
	    }

	  /* Vector subscripts need copying and are handled elsewhere.  */
	  if (info->ref)
	    gcc_assert (info->ref->u.ar.dimen_type[n] == DIMEN_RANGE);

	  /* look for the corresponding scalarizer dimension: dim.  */
	  for (dim = 0; dim < ndim; dim++)
	    if (ss->dim[dim] == n)
	      break;

	  /* loop exited early: the DIM being looked for has been found.  */
	  gcc_assert (dim < ndim);

	  /* Set the new lower bound.  */
	  from = loop.from[dim];
	  to = loop.to[dim];

	  gfc_conv_descriptor_lbound_set (&loop.pre, parm,
					  gfc_rank_cst[dim], from);

	  /* Set the new upper bound.  */
	  gfc_conv_descriptor_ubound_set (&loop.pre, parm,
					  gfc_rank_cst[dim], to);

	  /* Multiply the stride by the section stride to get the
	     total stride.  */
	  stride = fold_build2_loc (input_location, MULT_EXPR,
				    gfc_array_index_type,
				    stride, info->stride[n]);

	  tmp = fold_build2_loc (input_location, MULT_EXPR,
				 TREE_TYPE (offset), stride, from);
	  offset = fold_build2_loc (input_location, MINUS_EXPR,
				   TREE_TYPE (offset), offset, tmp);

	  /* Store the new stride.  */
	  gfc_conv_descriptor_stride_set (&loop.pre, parm,
					  gfc_rank_cst[dim], stride);
	}

      for (n = loop.dimen; n < loop.dimen + codim; n++)
	{
	  from = loop.from[n];
	  to = loop.to[n];
	  gfc_conv_descriptor_lbound_set (&loop.pre, parm,
					  gfc_rank_cst[n], from);
	  if (n < loop.dimen + codim - 1)
	    gfc_conv_descriptor_ubound_set (&loop.pre, parm,
					    gfc_rank_cst[n], to);
	}

      if (se->data_not_needed)
	gfc_conv_descriptor_data_set (&loop.pre, parm,
				      gfc_index_zero_node);
      else
	/* Point the data pointer at the 1st element in the section.  */
	gfc_get_dataptr_offset (&loop.pre, parm, desc, base,
				subref_array_target, expr);

      gfc_conv_descriptor_offset_set (&loop.pre, parm, offset);

      desc = parm;
    }

  /* For class arrays add the class tree into the saved descriptor to
     enable getting of _vptr and the like.  */
  if (expr->expr_type == EXPR_VARIABLE && VAR_P (desc)
      && IS_CLASS_ARRAY (expr->symtree->n.sym))
    {
      gfc_allocate_lang_decl (desc);
      GFC_DECL_SAVED_DESCRIPTOR (desc) =
	  DECL_LANG_SPECIFIC (expr->symtree->n.sym->backend_decl) ?
	    GFC_DECL_SAVED_DESCRIPTOR (expr->symtree->n.sym->backend_decl)
	  : expr->symtree->n.sym->backend_decl;
    }
  else if (expr->expr_type == EXPR_ARRAY && VAR_P (desc)
	   && IS_CLASS_ARRAY (expr))
    {
      tree vtype;
      gfc_allocate_lang_decl (desc);
      tmp = gfc_create_var (expr->ts.u.derived->backend_decl, "class");
      GFC_DECL_SAVED_DESCRIPTOR (desc) = tmp;
      vtype = gfc_class_vptr_get (tmp);
      gfc_add_modify (&se->pre, vtype,
		      gfc_build_addr_expr (TREE_TYPE (vtype),
				      gfc_find_vtab (&expr->ts)->backend_decl));
    }
  if (!se->direct_byref || se->byref_noassign)
    {
      /* Get a pointer to the new descriptor.  */
      if (se->want_pointer)
	se->expr = gfc_build_addr_expr (NULL_TREE, desc);
      else
	se->expr = desc;
    }

  gfc_add_block_to_block (&se->pre, &loop.pre);
  gfc_add_block_to_block (&se->post, &loop.post);

  /* Cleanup the scalarizer.  */
  gfc_cleanup_loop (&loop);
}


/* Calculate the array size (number of elements); if dim != NULL_TREE,
   return size for that dim (dim=0..rank-1; only for GFC_DESCRIPTOR_TYPE_P).  */
tree
gfc_tree_array_size (stmtblock_t *block, tree desc, gfc_expr *expr, tree dim)
{
  if (GFC_ARRAY_TYPE_P (TREE_TYPE (desc)))
    {
      gcc_assert (dim == NULL_TREE);
      return GFC_TYPE_ARRAY_SIZE (TREE_TYPE (desc));
    }
  tree size, tmp, rank = NULL_TREE, cond = NULL_TREE;
  symbol_attribute attr = gfc_expr_attr (expr);
  gfc_array_spec *as = gfc_get_full_arrayspec_from_expr (expr);
  gcc_assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)));
  if ((!attr.pointer && !attr.allocatable && as && as->type == AS_ASSUMED_RANK)
       || !dim)
    {
      if (expr->rank < 0)
	rank = fold_convert (signed_char_type_node,
			     gfc_conv_descriptor_rank (desc));
      else
	rank = build_int_cst (signed_char_type_node, expr->rank);
    }

  if (dim || expr->rank == 1)
    {
      if (!dim)
	dim = gfc_index_zero_node;
      tree ubound = gfc_conv_descriptor_ubound_get (desc, dim);
      tree lbound = gfc_conv_descriptor_lbound_get (desc, dim);

      size = fold_build2_loc (input_location, MINUS_EXPR,
			      gfc_array_index_type, ubound, lbound);
      size = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			      size, gfc_index_one_node);
      /* if (!allocatable && !pointer && assumed rank)
	   size = (idx == rank && ubound[rank-1] == -1 ? -1 : size;
	 else
	   size = max (0, size);  */
      size = fold_build2_loc (input_location, MAX_EXPR, gfc_array_index_type,
			      size, gfc_index_zero_node);
      if (!attr.pointer && !attr.allocatable
	  && as && as->type == AS_ASSUMED_RANK)
	{
	  tmp = fold_build2_loc (input_location, MINUS_EXPR, signed_char_type_node,
				 rank, build_int_cst (signed_char_type_node, 1));
	  cond = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				  fold_convert (signed_char_type_node, dim),
				  tmp);
	  tmp = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				 gfc_conv_descriptor_ubound_get (desc, dim),
				 build_int_cst (gfc_array_index_type, -1));
	  cond = fold_build2_loc (input_location, TRUTH_AND_EXPR, boolean_type_node,
				  cond, tmp);
	  tmp = build_int_cst (gfc_array_index_type, -1);
	  size = build3_loc (input_location, COND_EXPR, gfc_array_index_type,
			     cond, tmp, size);
	}
      return size;
    }

  /* size = 1. */
  size = gfc_create_var (gfc_array_index_type, "size");
  gfc_add_modify (block, size, build_int_cst (TREE_TYPE (size), 1));
  tree extent = gfc_create_var (gfc_array_index_type, "extent");

  stmtblock_t cond_block, loop_body;
  gfc_init_block (&cond_block);
  gfc_init_block (&loop_body);

  /* Loop: for (i = 0; i < rank; ++i).  */
  tree idx = gfc_create_var (signed_char_type_node, "idx");
  /* Loop body.  */
  /* #if (assumed-rank + !allocatable && !pointer)
       if (idx == rank - 1 && dim[idx].ubound == -1)
	 extent = -1;
       else
     #endif
	 extent = gfc->dim[i].ubound - gfc->dim[i].lbound + 1
	 if (extent < 0)
	   extent = 0
      size *= extent.  */
  cond = NULL_TREE;
  if (!attr.pointer && !attr.allocatable && as && as->type == AS_ASSUMED_RANK)
    {
      tmp = fold_build2_loc (input_location, MINUS_EXPR, signed_char_type_node,
			     rank, build_int_cst (signed_char_type_node, 1));
      cond = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				  idx, tmp);
      tmp = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
			     gfc_conv_descriptor_ubound_get (desc, idx),
			     build_int_cst (gfc_array_index_type, -1));
      cond = fold_build2_loc (input_location, TRUTH_AND_EXPR, boolean_type_node,
			      cond, tmp);
    }
  tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			 gfc_conv_descriptor_ubound_get (desc, idx),
			 gfc_conv_descriptor_lbound_get (desc, idx));
  tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			 tmp, gfc_index_one_node);
  gfc_add_modify (&cond_block, extent, tmp);
  tmp = fold_build2_loc (input_location, LT_EXPR, boolean_type_node,
			 extent, gfc_index_zero_node);
  tmp = build3_v (COND_EXPR, tmp,
		  fold_build2_loc (input_location, MODIFY_EXPR,
				   gfc_array_index_type,
				   extent, gfc_index_zero_node),
		  build_empty_stmt (input_location));
  gfc_add_expr_to_block (&cond_block, tmp);
  tmp = gfc_finish_block (&cond_block);
  if (cond)
    tmp = build3_v (COND_EXPR, cond,
		    fold_build2_loc (input_location, MODIFY_EXPR,
				     gfc_array_index_type, extent,
				     build_int_cst (gfc_array_index_type, -1)),
		    tmp);
   gfc_add_expr_to_block (&loop_body, tmp);
   /* size *= extent.  */
   gfc_add_modify (&loop_body, size,
		   fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
				    size, extent));
  /* Generate loop. */
  gfc_simple_for_loop (block, idx, build_int_cst (TREE_TYPE (idx), 0), rank, LT_EXPR,
		       build_int_cst (TREE_TYPE (idx), 1),
		       gfc_finish_block (&loop_body));
  return size;
}

/* Helper function for gfc_conv_array_parameter if array size needs to be
   computed.  */

static void
array_parameter_size (stmtblock_t *block, tree desc, gfc_expr *expr, tree *size)
{
  tree elem;
  *size = gfc_tree_array_size (block, desc, expr, NULL);
  elem = TYPE_SIZE_UNIT (gfc_get_element_type (TREE_TYPE (desc)));
  *size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			   *size, fold_convert (gfc_array_index_type, elem));
}

/* Helper function - return true if the argument is a pointer.  */

static bool
is_pointer (gfc_expr *e)
{
  gfc_symbol *sym;

  if (e->expr_type != EXPR_VARIABLE ||  e->symtree == NULL)
    return false;

  sym = e->symtree->n.sym;
  if (sym == NULL)
    return false;

  return sym->attr.pointer || sym->attr.proc_pointer;
}

/* Convert an array for passing as an actual parameter.  */

void
gfc_conv_array_parameter (gfc_se * se, gfc_expr * expr, bool g77,
			  const gfc_symbol *fsym, const char *proc_name,
			  tree *size)
{
  tree ptr;
  tree desc;
  tree tmp = NULL_TREE;
  tree stmt;
  tree parent = DECL_CONTEXT (current_function_decl);
  bool full_array_var;
  bool this_array_result;
  bool contiguous;
  bool no_pack;
  bool array_constructor;
  bool good_allocatable;
  bool ultimate_ptr_comp;
  bool ultimate_alloc_comp;
  gfc_symbol *sym;
  stmtblock_t block;
  gfc_ref *ref;

  ultimate_ptr_comp = false;
  ultimate_alloc_comp = false;

  for (ref = expr->ref; ref; ref = ref->next)
    {
      if (ref->next == NULL)
        break;

      if (ref->type == REF_COMPONENT)
	{
	  ultimate_ptr_comp = ref->u.c.component->attr.pointer;
	  ultimate_alloc_comp = ref->u.c.component->attr.allocatable;
	}
    }

  full_array_var = false;
  contiguous = false;

  if (expr->expr_type == EXPR_VARIABLE && ref && !ultimate_ptr_comp)
    full_array_var = gfc_full_array_ref_p (ref, &contiguous);

  sym = full_array_var ? expr->symtree->n.sym : NULL;

  /* The symbol should have an array specification.  */
  gcc_assert (!sym || sym->as || ref->u.ar.as);

  if (expr->expr_type == EXPR_ARRAY && expr->ts.type == BT_CHARACTER)
    {
      get_array_ctor_strlen (&se->pre, expr->value.constructor, &tmp);
      expr->ts.u.cl->backend_decl = tmp;
      se->string_length = tmp;
    }

  /* Is this the result of the enclosing procedure?  */
  this_array_result = (full_array_var && sym->attr.flavor == FL_PROCEDURE);
  if (this_array_result
	&& (sym->backend_decl != current_function_decl)
	&& (sym->backend_decl != parent))
    this_array_result = false;

  /* Passing an optional dummy argument as actual to an optional dummy?  */
  bool pass_optional;
  pass_optional = fsym && fsym->attr.optional && sym && sym->attr.optional;

  /* Passing address of the array if it is not pointer or assumed-shape.  */
  if (full_array_var && g77 && !this_array_result
      && sym->ts.type != BT_DERIVED && sym->ts.type != BT_CLASS)
    {
      tmp = gfc_get_symbol_decl (sym);

      if (sym->ts.type == BT_CHARACTER)
	se->string_length = sym->ts.u.cl->backend_decl;

      if (!sym->attr.pointer
	  && sym->as
	  && sym->as->type != AS_ASSUMED_SHAPE
	  && sym->as->type != AS_DEFERRED
	  && sym->as->type != AS_ASSUMED_RANK
	  && !sym->attr.allocatable)
        {
	  /* Some variables are declared directly, others are declared as
	     pointers and allocated on the heap.  */
          if (sym->attr.dummy || POINTER_TYPE_P (TREE_TYPE (tmp)))
            se->expr = tmp;
          else
	    se->expr = gfc_build_addr_expr (NULL_TREE, tmp);
	  if (size)
	    array_parameter_size (&se->pre, tmp, expr, size);
	  return;
        }

      if (sym->attr.allocatable)
        {
	  if (sym->attr.dummy || sym->attr.result)
	    {
	      gfc_conv_expr_descriptor (se, expr);
	      tmp = se->expr;
	    }
	  if (size)
	    array_parameter_size (&se->pre, tmp, expr, size);
	  se->expr = gfc_conv_array_data (tmp);
	  if (pass_optional)
	    {
	      tree cond = gfc_conv_expr_present (sym);
	      se->expr = build3_loc (input_location, COND_EXPR,
				     TREE_TYPE (se->expr), cond, se->expr,
				     fold_convert (TREE_TYPE (se->expr),
						   null_pointer_node));
	    }
          return;
        }
    }

  /* A convenient reduction in scope.  */
  contiguous = g77 && !this_array_result && contiguous;

  /* There is no need to pack and unpack the array, if it is contiguous
     and not a deferred- or assumed-shape array, or if it is simply
     contiguous.  */
  no_pack = ((sym && sym->as
		  && !sym->attr.pointer
		  && sym->as->type != AS_DEFERRED
		  && sym->as->type != AS_ASSUMED_RANK
		  && sym->as->type != AS_ASSUMED_SHAPE)
		      ||
	     (ref && ref->u.ar.as
		  && ref->u.ar.as->type != AS_DEFERRED
		  && ref->u.ar.as->type != AS_ASSUMED_RANK
		  && ref->u.ar.as->type != AS_ASSUMED_SHAPE)
		      ||
	     gfc_is_simply_contiguous (expr, false, true));

  no_pack = contiguous && no_pack;

  /* If we have an EXPR_OP or a function returning an explicit-shaped
     or allocatable array, an array temporary will be generated which
     does not need to be packed / unpacked if passed to an
     explicit-shape dummy array.  */

  if (g77)
    {
      if (expr->expr_type == EXPR_OP)
	no_pack = 1;
      else if (expr->expr_type == EXPR_FUNCTION && expr->value.function.esym)
	{
	  gfc_symbol *result = expr->value.function.esym->result;
	  if (result->attr.dimension
	      && (result->as->type == AS_EXPLICIT
		  || result->attr.allocatable
		  || result->attr.contiguous))
	    no_pack = 1;
	}
    }

  /* Array constructors are always contiguous and do not need packing.  */
  array_constructor = g77 && !this_array_result && expr->expr_type == EXPR_ARRAY;

  /* Same is true of contiguous sections from allocatable variables.  */
  good_allocatable = contiguous
		       && expr->symtree
		       && expr->symtree->n.sym->attr.allocatable;

  /* Or ultimate allocatable components.  */
  ultimate_alloc_comp = contiguous && ultimate_alloc_comp;

  if (no_pack || array_constructor || good_allocatable || ultimate_alloc_comp)
    {
      gfc_conv_expr_descriptor (se, expr);
      /* Deallocate the allocatable components of structures that are
	 not variable.  */
      if ((expr->ts.type == BT_DERIVED || expr->ts.type == BT_CLASS)
	   && expr->ts.u.derived->attr.alloc_comp
	   && expr->expr_type != EXPR_VARIABLE)
	{
	  tmp = gfc_deallocate_alloc_comp (expr->ts.u.derived, se->expr, expr->rank);

	  /* The components shall be deallocated before their containing entity.  */
	  gfc_prepend_expr_to_block (&se->post, tmp);
	}
      if (expr->ts.type == BT_CHARACTER && expr->expr_type != EXPR_FUNCTION)
	se->string_length = expr->ts.u.cl->backend_decl;
      if (size)
	array_parameter_size (&se->pre, se->expr, expr, size);
      se->expr = gfc_conv_array_data (se->expr);
      return;
    }

  if (this_array_result)
    {
      /* Result of the enclosing function.  */
      gfc_conv_expr_descriptor (se, expr);
      if (size)
	array_parameter_size (&se->pre, se->expr, expr, size);
      se->expr = gfc_build_addr_expr (NULL_TREE, se->expr);

      if (g77 && TREE_TYPE (TREE_TYPE (se->expr)) != NULL_TREE
	      && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (TREE_TYPE (se->expr))))
	se->expr = gfc_conv_array_data (build_fold_indirect_ref_loc (input_location,
								 se->expr));

      return;
    }
  else
    {
      /* Every other type of array.  */
      se->want_pointer = 1;
      gfc_conv_expr_descriptor (se, expr);

      if (size)
	array_parameter_size (&se->pre,
			      build_fold_indirect_ref_loc (input_location,
							    se->expr),
			      expr, size);
    }

  /* Deallocate the allocatable components of structures that are
     not variable, for descriptorless arguments.
     Arguments with a descriptor are handled in gfc_conv_procedure_call.  */
  if (g77 && (expr->ts.type == BT_DERIVED || expr->ts.type == BT_CLASS)
	  && expr->ts.u.derived->attr.alloc_comp
	  && expr->expr_type != EXPR_VARIABLE)
    {
      tmp = build_fold_indirect_ref_loc (input_location, se->expr);
      tmp = gfc_deallocate_alloc_comp (expr->ts.u.derived, tmp, expr->rank);

      /* The components shall be deallocated before their containing entity.  */
      gfc_prepend_expr_to_block (&se->post, tmp);
    }

  if (g77 || (fsym && fsym->attr.contiguous
	      && !gfc_is_simply_contiguous (expr, false, true)))
    {
      tree origptr = NULL_TREE;

      desc = se->expr;

      /* For contiguous arrays, save the original value of the descriptor.  */
      if (!g77)
	{
	  origptr = gfc_create_var (pvoid_type_node, "origptr");
	  tmp = build_fold_indirect_ref_loc (input_location, desc);
	  tmp = gfc_conv_array_data (tmp);
	  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
				 TREE_TYPE (origptr), origptr,
				 fold_convert (TREE_TYPE (origptr), tmp));
	  gfc_add_expr_to_block (&se->pre, tmp);
	}

      /* Repack the array.  */
      if (warn_array_temporaries)
	{
	  if (fsym)
	    gfc_warning (OPT_Warray_temporaries,
			 "Creating array temporary at %L for argument %qs",
			 &expr->where, fsym->name);
	  else
	    gfc_warning (OPT_Warray_temporaries,
			 "Creating array temporary at %L", &expr->where);
	}

      /* When optimizing, we can use gfc_conv_subref_array_arg for
	 making the packing and unpacking operation visible to the
	 optimizers.  */

      if (g77 && flag_inline_arg_packing && expr->expr_type == EXPR_VARIABLE
	  && !is_pointer (expr) && ! gfc_has_dimen_vector_ref (expr)
	  && !(expr->symtree->n.sym->as
	       && expr->symtree->n.sym->as->type == AS_ASSUMED_RANK)
	  && (fsym == NULL || fsym->ts.type != BT_ASSUMED))
	{
	  gfc_conv_subref_array_arg (se, expr, g77,
				     fsym ? fsym->attr.intent : INTENT_INOUT,
				     false, fsym, proc_name, sym, true);
	  return;
	}

      ptr = build_call_expr_loc (input_location,
			     gfor_fndecl_in_pack, 1, desc);

      if (fsym && fsym->attr.optional && sym && sym->attr.optional)
	{
	  tmp = gfc_conv_expr_present (sym);
	  ptr = build3_loc (input_location, COND_EXPR, TREE_TYPE (se->expr),
			tmp, fold_convert (TREE_TYPE (se->expr), ptr),
			fold_convert (TREE_TYPE (se->expr), null_pointer_node));
	}

      ptr = gfc_evaluate_now (ptr, &se->pre);

      /* Use the packed data for the actual argument, except for contiguous arrays,
	 where the descriptor's data component is set.  */
      if (g77)
	se->expr = ptr;
      else
	{
	  tmp = build_fold_indirect_ref_loc (input_location, desc);

	  gfc_ss * ss = gfc_walk_expr (expr);
	  if (!transposed_dims (ss))
	    gfc_conv_descriptor_data_set (&se->pre, tmp, ptr);
	  else
	    {
	      tree old_field, new_field;

	      /* The original descriptor has transposed dims so we can't reuse
		 it directly; we have to create a new one.  */
	      tree old_desc = tmp;
	      tree new_desc = gfc_create_var (TREE_TYPE (old_desc), "arg_desc");

	      old_field = gfc_conv_descriptor_dtype (old_desc);
	      new_field = gfc_conv_descriptor_dtype (new_desc);
	      gfc_add_modify (&se->pre, new_field, old_field);

	      old_field = gfc_conv_descriptor_offset (old_desc);
	      new_field = gfc_conv_descriptor_offset (new_desc);
	      gfc_add_modify (&se->pre, new_field, old_field);

	      for (int i = 0; i < expr->rank; i++)
		{
		  old_field = gfc_conv_descriptor_dimension (old_desc,
			gfc_rank_cst[get_array_ref_dim_for_loop_dim (ss, i)]);
		  new_field = gfc_conv_descriptor_dimension (new_desc,
			gfc_rank_cst[i]);
		  gfc_add_modify (&se->pre, new_field, old_field);
		}

	      if (flag_coarray == GFC_FCOARRAY_LIB
		  && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (old_desc))
		  && GFC_TYPE_ARRAY_AKIND (TREE_TYPE (old_desc))
		     == GFC_ARRAY_ALLOCATABLE)
		{
		  old_field = gfc_conv_descriptor_token (old_desc);
		  new_field = gfc_conv_descriptor_token (new_desc);
		  gfc_add_modify (&se->pre, new_field, old_field);
		}

	      gfc_conv_descriptor_data_set (&se->pre, new_desc, ptr);
	      se->expr = gfc_build_addr_expr (NULL_TREE, new_desc);
	    }
	  gfc_free_ss (ss);
	}

      if (gfc_option.rtcheck & GFC_RTCHECK_ARRAY_TEMPS)
	{
	  char * msg;

	  if (fsym && proc_name)
	    msg = xasprintf ("An array temporary was created for argument "
			     "'%s' of procedure '%s'", fsym->name, proc_name);
	  else
	    msg = xasprintf ("An array temporary was created");

	  tmp = build_fold_indirect_ref_loc (input_location,
					 desc);
	  tmp = gfc_conv_array_data (tmp);
	  tmp = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				 fold_convert (TREE_TYPE (tmp), ptr), tmp);

	  if (pass_optional)
	    tmp = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
				   logical_type_node,
				   gfc_conv_expr_present (sym), tmp);

	  gfc_trans_runtime_check (false, true, tmp, &se->pre,
				   &expr->where, msg);
	  free (msg);
	}

      gfc_start_block (&block);

      /* Copy the data back.  */
      if (fsym == NULL || fsym->attr.intent != INTENT_IN)
	{
	  tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_in_unpack, 2, desc, ptr);
	  gfc_add_expr_to_block (&block, tmp);
	}

      /* Free the temporary.  */
      tmp = gfc_call_free (ptr);
      gfc_add_expr_to_block (&block, tmp);

      stmt = gfc_finish_block (&block);

      gfc_init_block (&block);
      /* Only if it was repacked.  This code needs to be executed before the
         loop cleanup code.  */
      tmp = build_fold_indirect_ref_loc (input_location,
				     desc);
      tmp = gfc_conv_array_data (tmp);
      tmp = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			     fold_convert (TREE_TYPE (tmp), ptr), tmp);

      if (pass_optional)
	tmp = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			       logical_type_node,
			       gfc_conv_expr_present (sym), tmp);

      tmp = build3_v (COND_EXPR, tmp, stmt, build_empty_stmt (input_location));

      gfc_add_expr_to_block (&block, tmp);
      gfc_add_block_to_block (&block, &se->post);

      gfc_init_block (&se->post);

      /* Reset the descriptor pointer.  */
      if (!g77)
        {
          tmp = build_fold_indirect_ref_loc (input_location, desc);
          gfc_conv_descriptor_data_set (&se->post, tmp, origptr);
        }

      gfc_add_block_to_block (&se->post, &block);
    }
}


/* This helper function calculates the size in words of a full array.  */

tree
gfc_full_array_size (stmtblock_t *block, tree decl, int rank)
{
  tree idx;
  tree nelems;
  tree tmp;
  idx = gfc_rank_cst[rank - 1];
  nelems = gfc_conv_descriptor_ubound_get (decl, idx);
  tmp = gfc_conv_descriptor_lbound_get (decl, idx);
  tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			 nelems, tmp);
  tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			 tmp, gfc_index_one_node);
  tmp = gfc_evaluate_now (tmp, block);

  nelems = gfc_conv_descriptor_stride_get (decl, idx);
  tmp = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			 nelems, tmp);
  return gfc_evaluate_now (tmp, block);
}


/* Allocate dest to the same size as src, and copy src -> dest.
   If no_malloc is set, only the copy is done.  */

static tree
duplicate_allocatable (tree dest, tree src, tree type, int rank,
		       bool no_malloc, bool no_memcpy, tree str_sz,
		       tree add_when_allocated)
{
  tree tmp;
  tree eltype;
  tree size;
  tree nelems;
  tree null_cond;
  tree null_data;
  stmtblock_t block;

  /* If the source is null, set the destination to null.  Then,
     allocate memory to the destination.  */
  gfc_init_block (&block);

  if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (dest)))
    {
      gfc_add_modify (&block, dest, fold_convert (type, null_pointer_node));
      null_data = gfc_finish_block (&block);

      gfc_init_block (&block);
      eltype = TREE_TYPE (type);
      if (str_sz != NULL_TREE)
	size = str_sz;
      else
	size = TYPE_SIZE_UNIT (eltype);

      if (!no_malloc)
	{
	  tmp = gfc_call_malloc (&block, type, size);
	  gfc_add_modify (&block, dest, fold_convert (type, tmp));
	}

      if (!no_memcpy)
	{
	  tmp = builtin_decl_explicit (BUILT_IN_MEMCPY);
	  tmp = build_call_expr_loc (input_location, tmp, 3, dest, src,
				     fold_convert (size_type_node, size));
	  gfc_add_expr_to_block (&block, tmp);
	}
    }
  else
    {
      gfc_conv_descriptor_data_set (&block, dest, null_pointer_node);
      null_data = gfc_finish_block (&block);

      gfc_init_block (&block);
      if (rank)
	nelems = gfc_full_array_size (&block, src, rank);
      else
	nelems = gfc_index_one_node;

      /* If type is not the array type, then it is the element type.  */
      if (GFC_ARRAY_TYPE_P (type) || GFC_DESCRIPTOR_TYPE_P (type))
	eltype = gfc_get_element_type (type);
      else
	eltype = type;

      if (str_sz != NULL_TREE)
	tmp = fold_convert (gfc_array_index_type, str_sz);
      else
	tmp = fold_convert (gfc_array_index_type,
			    TYPE_SIZE_UNIT (eltype));

      tmp = gfc_evaluate_now (tmp, &block);
      size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			      nelems, tmp);
      if (!no_malloc)
	{
	  tmp = TREE_TYPE (gfc_conv_descriptor_data_get (src));
	  tmp = gfc_call_malloc (&block, tmp, size);
	  gfc_conv_descriptor_data_set (&block, dest, tmp);
	}

      /* We know the temporary and the value will be the same length,
	 so can use memcpy.  */
      if (!no_memcpy)
	{
	  tmp = builtin_decl_explicit (BUILT_IN_MEMCPY);
	  tmp = build_call_expr_loc (input_location, tmp, 3,
				     gfc_conv_descriptor_data_get (dest),
				     gfc_conv_descriptor_data_get (src),
				     fold_convert (size_type_node, size));
	  gfc_add_expr_to_block (&block, tmp);
	}
    }

  gfc_add_expr_to_block (&block, add_when_allocated);
  tmp = gfc_finish_block (&block);

  /* Null the destination if the source is null; otherwise do
     the allocate and copy.  */
  if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (src)))
    null_cond = src;
  else
    null_cond = gfc_conv_descriptor_data_get (src);

  null_cond = convert (pvoid_type_node, null_cond);
  null_cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			       null_cond, null_pointer_node);
  return build3_v (COND_EXPR, null_cond, tmp, null_data);
}


/* Allocate dest to the same size as src, and copy data src -> dest.  */

tree
gfc_duplicate_allocatable (tree dest, tree src, tree type, int rank,
			   tree add_when_allocated)
{
  return duplicate_allocatable (dest, src, type, rank, false, false,
				NULL_TREE, add_when_allocated);
}


/* Copy data src -> dest.  */

tree
gfc_copy_allocatable_data (tree dest, tree src, tree type, int rank)
{
  return duplicate_allocatable (dest, src, type, rank, true, false,
				NULL_TREE, NULL_TREE);
}

/* Allocate dest to the same size as src, but don't copy anything.  */

tree
gfc_duplicate_allocatable_nocopy (tree dest, tree src, tree type, int rank)
{
  return duplicate_allocatable (dest, src, type, rank, false, true,
				NULL_TREE, NULL_TREE);
}


static tree
duplicate_allocatable_coarray (tree dest, tree dest_tok, tree src,
			       tree type, int rank)
{
  tree tmp;
  tree size;
  tree nelems;
  tree null_cond;
  tree null_data;
  stmtblock_t block, globalblock;

  /* If the source is null, set the destination to null.  Then,
     allocate memory to the destination.  */
  gfc_init_block (&block);
  gfc_init_block (&globalblock);

  if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (dest)))
    {
      gfc_se se;
      symbol_attribute attr;
      tree dummy_desc;

      gfc_init_se (&se, NULL);
      gfc_clear_attr (&attr);
      attr.allocatable = 1;
      dummy_desc = gfc_conv_scalar_to_descriptor (&se, dest, attr);
      gfc_add_block_to_block (&globalblock, &se.pre);
      size = TYPE_SIZE_UNIT (TREE_TYPE (type));

      gfc_add_modify (&block, dest, fold_convert (type, null_pointer_node));
      gfc_allocate_using_caf_lib (&block, dummy_desc, size,
				  gfc_build_addr_expr (NULL_TREE, dest_tok),
				  NULL_TREE, NULL_TREE, NULL_TREE,
				  GFC_CAF_COARRAY_ALLOC_REGISTER_ONLY);
      null_data = gfc_finish_block (&block);

      gfc_init_block (&block);

      gfc_allocate_using_caf_lib (&block, dummy_desc,
				  fold_convert (size_type_node, size),
				  gfc_build_addr_expr (NULL_TREE, dest_tok),
				  NULL_TREE, NULL_TREE, NULL_TREE,
				  GFC_CAF_COARRAY_ALLOC);

      tmp = builtin_decl_explicit (BUILT_IN_MEMCPY);
      tmp = build_call_expr_loc (input_location, tmp, 3, dest, src,
				 fold_convert (size_type_node, size));
      gfc_add_expr_to_block (&block, tmp);
    }
  else
    {
      /* Set the rank or unitialized memory access may be reported.  */
      tmp = gfc_conv_descriptor_rank (dest);
      gfc_add_modify (&globalblock, tmp, build_int_cst (TREE_TYPE (tmp), rank));

      if (rank)
	nelems = gfc_full_array_size (&block, src, rank);
      else
	nelems = integer_one_node;

      tmp = fold_convert (size_type_node,
			  TYPE_SIZE_UNIT (gfc_get_element_type (type)));
      size = fold_build2_loc (input_location, MULT_EXPR, size_type_node,
			      fold_convert (size_type_node, nelems), tmp);

      gfc_conv_descriptor_data_set (&block, dest, null_pointer_node);
      gfc_allocate_using_caf_lib (&block, dest, fold_convert (size_type_node,
							      size),
				  gfc_build_addr_expr (NULL_TREE, dest_tok),
				  NULL_TREE, NULL_TREE, NULL_TREE,
				  GFC_CAF_COARRAY_ALLOC_REGISTER_ONLY);
      null_data = gfc_finish_block (&block);

      gfc_init_block (&block);
      gfc_allocate_using_caf_lib (&block, dest,
				  fold_convert (size_type_node, size),
				  gfc_build_addr_expr (NULL_TREE, dest_tok),
				  NULL_TREE, NULL_TREE, NULL_TREE,
				  GFC_CAF_COARRAY_ALLOC);

      tmp = builtin_decl_explicit (BUILT_IN_MEMCPY);
      tmp = build_call_expr_loc (input_location, tmp, 3,
				 gfc_conv_descriptor_data_get (dest),
				 gfc_conv_descriptor_data_get (src),
				 fold_convert (size_type_node, size));
      gfc_add_expr_to_block (&block, tmp);
    }

  tmp = gfc_finish_block (&block);

  /* Null the destination if the source is null; otherwise do
     the register and copy.  */
  if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (src)))
    null_cond = src;
  else
    null_cond = gfc_conv_descriptor_data_get (src);

  null_cond = convert (pvoid_type_node, null_cond);
  null_cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			       null_cond, null_pointer_node);
  gfc_add_expr_to_block (&globalblock, build3_v (COND_EXPR, null_cond, tmp,
						 null_data));
  return gfc_finish_block (&globalblock);
}


/* Helper function to abstract whether coarray processing is enabled.  */

static bool
caf_enabled (int caf_mode)
{
  return (caf_mode & GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY)
      == GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY;
}


/* Helper function to abstract whether coarray processing is enabled
   and we are in a derived type coarray.  */

static bool
caf_in_coarray (int caf_mode)
{
  static const int pat = GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY
			 | GFC_STRUCTURE_CAF_MODE_IN_COARRAY;
  return (caf_mode & pat) == pat;
}


/* Helper function to abstract whether coarray is to deallocate only.  */

bool
gfc_caf_is_dealloc_only (int caf_mode)
{
  return (caf_mode & GFC_STRUCTURE_CAF_MODE_DEALLOC_ONLY)
      == GFC_STRUCTURE_CAF_MODE_DEALLOC_ONLY;
}


/* Recursively traverse an object of derived type, generating code to
   deallocate, nullify or copy allocatable components.  This is the work horse
   function for the functions named in this enum.  */

enum {DEALLOCATE_ALLOC_COMP = 1, NULLIFY_ALLOC_COMP,
      COPY_ALLOC_COMP, COPY_ONLY_ALLOC_COMP, REASSIGN_CAF_COMP,
      ALLOCATE_PDT_COMP, DEALLOCATE_PDT_COMP, CHECK_PDT_DUMMY,
      BCAST_ALLOC_COMP};

static gfc_actual_arglist *pdt_param_list;

static tree
structure_alloc_comps (gfc_symbol * der_type, tree decl, tree dest,
		       int rank, int purpose, int caf_mode,
		       gfc_co_subroutines_args *args,
		       bool no_finalization = false)
{
  gfc_component *c;
  gfc_loopinfo loop;
  stmtblock_t fnblock;
  stmtblock_t loopbody;
  stmtblock_t tmpblock;
  tree decl_type;
  tree tmp;
  tree comp;
  tree dcmp;
  tree nelems;
  tree index;
  tree var;
  tree cdecl;
  tree ctype;
  tree vref, dref;
  tree null_cond = NULL_TREE;
  tree add_when_allocated;
  tree dealloc_fndecl;
  tree caf_token;
  gfc_symbol *vtab;
  int caf_dereg_mode;
  symbol_attribute *attr;
  bool deallocate_called;

  gfc_init_block (&fnblock);

  decl_type = TREE_TYPE (decl);

  if ((POINTER_TYPE_P (decl_type))
	|| (TREE_CODE (decl_type) == REFERENCE_TYPE && rank == 0))
    {
      decl = build_fold_indirect_ref_loc (input_location, decl);
      /* Deref dest in sync with decl, but only when it is not NULL.  */
      if (dest)
	dest = build_fold_indirect_ref_loc (input_location, dest);

      /* Update the decl_type because it got dereferenced.  */
      decl_type = TREE_TYPE (decl);
    }

  /* If this is an array of derived types with allocatable components
     build a loop and recursively call this function.  */
  if (TREE_CODE (decl_type) == ARRAY_TYPE
      || (GFC_DESCRIPTOR_TYPE_P (decl_type) && rank != 0))
    {
      tmp = gfc_conv_array_data (decl);
      var = build_fold_indirect_ref_loc (input_location, tmp);

      /* Get the number of elements - 1 and set the counter.  */
      if (GFC_DESCRIPTOR_TYPE_P (decl_type))
	{
	  /* Use the descriptor for an allocatable array.  Since this
	     is a full array reference, we only need the descriptor
	     information from dimension = rank.  */
	  tmp = gfc_full_array_size (&fnblock, decl, rank);
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type, tmp,
				 gfc_index_one_node);

	  null_cond = gfc_conv_descriptor_data_get (decl);
	  null_cond = fold_build2_loc (input_location, NE_EXPR,
				       logical_type_node, null_cond,
				       build_int_cst (TREE_TYPE (null_cond), 0));
	}
      else
	{
	  /*  Otherwise use the TYPE_DOMAIN information.  */
	  tmp = array_type_nelts (decl_type);
	  tmp = fold_convert (gfc_array_index_type, tmp);
	}

      /* Remember that this is, in fact, the no. of elements - 1.  */
      nelems = gfc_evaluate_now (tmp, &fnblock);
      index = gfc_create_var (gfc_array_index_type, "S");

      /* Build the body of the loop.  */
      gfc_init_block (&loopbody);

      vref = gfc_build_array_ref (var, index, NULL);

      if (purpose == COPY_ALLOC_COMP || purpose == COPY_ONLY_ALLOC_COMP)
	{
	  tmp = build_fold_indirect_ref_loc (input_location,
					     gfc_conv_array_data (dest));
	  dref = gfc_build_array_ref (tmp, index, NULL);
	  tmp = structure_alloc_comps (der_type, vref, dref, rank,
				       COPY_ALLOC_COMP, caf_mode, args,
				       no_finalization);
	}
      else
	tmp = structure_alloc_comps (der_type, vref, NULL_TREE, rank, purpose,
				     caf_mode, args, no_finalization);

      gfc_add_expr_to_block (&loopbody, tmp);

      /* Build the loop and return.  */
      gfc_init_loopinfo (&loop);
      loop.dimen = 1;
      loop.from[0] = gfc_index_zero_node;
      loop.loopvar[0] = index;
      loop.to[0] = nelems;
      gfc_trans_scalarizing_loops (&loop, &loopbody);
      gfc_add_block_to_block (&fnblock, &loop.pre);

      tmp = gfc_finish_block (&fnblock);
      /* When copying allocateable components, the above implements the
	 deep copy.  Nevertheless is a deep copy only allowed, when the current
	 component is allocated, for which code will be generated in
	 gfc_duplicate_allocatable (), where the deep copy code is just added
	 into the if's body, by adding tmp (the deep copy code) as last
	 argument to gfc_duplicate_allocatable ().  */
      if (purpose == COPY_ALLOC_COMP
	  && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (dest)))
	tmp = gfc_duplicate_allocatable (dest, decl, decl_type, rank,
					 tmp);
      else if (null_cond != NULL_TREE)
	tmp = build3_v (COND_EXPR, null_cond, tmp,
			build_empty_stmt (input_location));

      return tmp;
    }

  if (purpose == DEALLOCATE_ALLOC_COMP && der_type->attr.pdt_type)
    {
      tmp = structure_alloc_comps (der_type, decl, NULL_TREE, rank,
				   DEALLOCATE_PDT_COMP, 0, args,
				   no_finalization);
      gfc_add_expr_to_block (&fnblock, tmp);
    }
  else if (purpose == ALLOCATE_PDT_COMP && der_type->attr.alloc_comp)
    {
      tmp = structure_alloc_comps (der_type, decl, NULL_TREE, rank,
				   NULLIFY_ALLOC_COMP, 0, args,
				   no_finalization);
      gfc_add_expr_to_block (&fnblock, tmp);
    }

  /* Still having a descriptor array of rank == 0 here, indicates an
     allocatable coarrays.  Dereference it correctly.  */
  if (GFC_DESCRIPTOR_TYPE_P (decl_type))
    {
      decl = build_fold_indirect_ref (gfc_conv_array_data (decl));
    }
  /* Otherwise, act on the components or recursively call self to
     act on a chain of components.  */
  for (c = der_type->components; c; c = c->next)
    {
      bool cmp_has_alloc_comps = (c->ts.type == BT_DERIVED
				  || c->ts.type == BT_CLASS)
				    && c->ts.u.derived->attr.alloc_comp;
      bool same_type = (c->ts.type == BT_DERIVED && der_type == c->ts.u.derived)
	|| (c->ts.type == BT_CLASS && der_type == CLASS_DATA (c)->ts.u.derived);

      bool is_pdt_type = c->ts.type == BT_DERIVED
			 && c->ts.u.derived->attr.pdt_type;

      cdecl = c->backend_decl;
      ctype = TREE_TYPE (cdecl);

      switch (purpose)
	{

	case BCAST_ALLOC_COMP:

	  tree ubound;
	  tree cdesc;
	  stmtblock_t derived_type_block;

	  gfc_init_block (&tmpblock);

	  comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				  decl, cdecl, NULL_TREE);

	  /* Shortcut to get the attributes of the component.  */
	  if (c->ts.type == BT_CLASS)
	    {
	      attr = &CLASS_DATA (c)->attr;
	      if (attr->class_pointer)
		continue;
	    }
	  else
	    {
	      attr = &c->attr;
	      if (attr->pointer)
		continue;
	    }

	  /* Do not broadcast a caf_token.  These are local to the image.  */
	  if (attr->caf_token)
	    continue;

	  add_when_allocated = NULL_TREE;
	  if (cmp_has_alloc_comps
	      && !c->attr.pointer && !c->attr.proc_pointer)
	    {
	      if (c->ts.type == BT_CLASS)
		{
		  rank = CLASS_DATA (c)->as ? CLASS_DATA (c)->as->rank : 0;
		  add_when_allocated
		      = structure_alloc_comps (CLASS_DATA (c)->ts.u.derived,
					       comp, NULL_TREE, rank, purpose,
					       caf_mode, args, no_finalization);
		}
	      else
		{
		  rank = c->as ? c->as->rank : 0;
		  add_when_allocated = structure_alloc_comps (c->ts.u.derived,
							      comp, NULL_TREE,
							      rank, purpose,
							      caf_mode, args,
							      no_finalization);
		}
	    }

	  gfc_init_block (&derived_type_block);
	  if (add_when_allocated)
	    gfc_add_expr_to_block (&derived_type_block, add_when_allocated);
	  tmp = gfc_finish_block (&derived_type_block);
	  gfc_add_expr_to_block (&tmpblock, tmp);

	  /* Convert the component into a rank 1 descriptor type.  */
	  if (attr->dimension)
	    {
	      tmp = gfc_get_element_type (TREE_TYPE (comp));
	      if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (comp)))
		ubound = GFC_TYPE_ARRAY_SIZE (TREE_TYPE (comp));
	      else
		ubound = gfc_full_array_size (&tmpblock, comp,
					      c->ts.type == BT_CLASS
					      ? CLASS_DATA (c)->as->rank
					      : c->as->rank);
	    }
	  else
	    {
	      tmp = TREE_TYPE (comp);
	      ubound = build_int_cst (gfc_array_index_type, 1);
	    }

	  /* Treat strings like arrays.  Or the other way around, do not
	   * generate an additional array layer for scalar components.  */
	  if (attr->dimension || c->ts.type == BT_CHARACTER)
	    {
	      cdesc = gfc_get_array_type_bounds (tmp, 1, 0, &gfc_index_one_node,
						 &ubound, 1,
						 GFC_ARRAY_ALLOCATABLE, false);

	      cdesc = gfc_create_var (cdesc, "cdesc");
	      DECL_ARTIFICIAL (cdesc) = 1;

	      gfc_add_modify (&tmpblock, gfc_conv_descriptor_dtype (cdesc),
			      gfc_get_dtype_rank_type (1, tmp));
	      gfc_conv_descriptor_lbound_set (&tmpblock, cdesc,
					      gfc_index_zero_node,
					      gfc_index_one_node);
	      gfc_conv_descriptor_stride_set (&tmpblock, cdesc,
					      gfc_index_zero_node,
					      gfc_index_one_node);
	      gfc_conv_descriptor_ubound_set (&tmpblock, cdesc,
					      gfc_index_zero_node, ubound);
	    }
	  else
	    /* Prevent warning.  */
	    cdesc = NULL_TREE;

	  if (attr->dimension)
	    {
	      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (comp)))
		comp = gfc_conv_descriptor_data_get (comp);
	      else
		comp = gfc_build_addr_expr (NULL_TREE, comp);
	    }
	  else
	    {
	      gfc_se se;

	      gfc_init_se (&se, NULL);

	      comp = gfc_conv_scalar_to_descriptor (&se, comp,
						     c->ts.type == BT_CLASS
						     ? CLASS_DATA (c)->attr
						     : c->attr);
	      if (c->ts.type == BT_CHARACTER)
		comp = gfc_build_addr_expr (NULL_TREE, comp);
	      gfc_add_block_to_block (&tmpblock, &se.pre);
	    }

	  if (attr->dimension || c->ts.type == BT_CHARACTER)
	    gfc_conv_descriptor_data_set (&tmpblock, cdesc, comp);
	  else
	    cdesc = comp;

	  tree fndecl;

	  fndecl = build_call_expr_loc (input_location,
					gfor_fndecl_co_broadcast, 5,
					gfc_build_addr_expr (pvoid_type_node,cdesc),
					args->image_index,
					null_pointer_node, null_pointer_node,
					null_pointer_node);

	  gfc_add_expr_to_block (&tmpblock, fndecl);
	  gfc_add_block_to_block (&fnblock, &tmpblock);

	  break;

	case DEALLOCATE_ALLOC_COMP:

	  gfc_init_block (&tmpblock);

	  comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				  decl, cdecl, NULL_TREE);

	  /* Shortcut to get the attributes of the component.  */
	  if (c->ts.type == BT_CLASS)
	    {
	      attr = &CLASS_DATA (c)->attr;
	      if (attr->class_pointer)
		continue;
	    }
	  else
	    {
	      attr = &c->attr;
	      if (attr->pointer)
		continue;
	    }

	  if (!no_finalization && ((c->ts.type == BT_DERIVED && !c->attr.pointer)
	     || (c->ts.type == BT_CLASS && !CLASS_DATA (c)->attr.class_pointer)))
	    /* Call the finalizer, which will free the memory and nullify the
	       pointer of an array.  */
	    deallocate_called = gfc_add_comp_finalizer_call (&tmpblock, comp, c,
							 caf_enabled (caf_mode))
		&& attr->dimension;
	  else
	    deallocate_called = false;

	  /* Add the _class ref for classes.  */
	  if (c->ts.type == BT_CLASS && attr->allocatable)
	    comp = gfc_class_data_get (comp);

	  add_when_allocated = NULL_TREE;
	  if (cmp_has_alloc_comps
	      && !c->attr.pointer && !c->attr.proc_pointer
	      && !same_type
	      && !deallocate_called)
	    {
	      /* Add checked deallocation of the components.  This code is
		 obviously added because the finalizer is not trusted to free
		 all memory.  */
	      if (c->ts.type == BT_CLASS)
		{
		  rank = CLASS_DATA (c)->as ? CLASS_DATA (c)->as->rank : 0;
		  add_when_allocated
		      = structure_alloc_comps (CLASS_DATA (c)->ts.u.derived,
					       comp, NULL_TREE, rank, purpose,
					       caf_mode, args, no_finalization);
		}
	      else
		{
		  rank = c->as ? c->as->rank : 0;
		  add_when_allocated = structure_alloc_comps (c->ts.u.derived,
							      comp, NULL_TREE,
							      rank, purpose,
							      caf_mode, args,
							      no_finalization);
		}
	    }

	  if (attr->allocatable && !same_type
	      && (!attr->codimension || caf_enabled (caf_mode)))
	    {
	      /* Handle all types of components besides components of the
		 same_type as the current one, because those would create an
		 endless loop.  */
	      caf_dereg_mode
		  = (caf_in_coarray (caf_mode) || attr->codimension)
		  ? (gfc_caf_is_dealloc_only (caf_mode)
		     ? GFC_CAF_COARRAY_DEALLOCATE_ONLY
		     : GFC_CAF_COARRAY_DEREGISTER)
		  : GFC_CAF_COARRAY_NOCOARRAY;

	      caf_token = NULL_TREE;
	      /* Coarray components are handled directly by
		 deallocate_with_status.  */
	      if (!attr->codimension
		  && caf_dereg_mode != GFC_CAF_COARRAY_NOCOARRAY)
		{
		  if (c->caf_token)
		    caf_token = fold_build3_loc (input_location, COMPONENT_REF,
						 TREE_TYPE (c->caf_token),
						 decl, c->caf_token, NULL_TREE);
		  else if (attr->dimension && !attr->proc_pointer)
		    caf_token = gfc_conv_descriptor_token (comp);
		}

	      tmp = gfc_deallocate_with_status (comp, NULL_TREE, NULL_TREE,
						NULL_TREE, NULL_TREE, true,
						NULL, caf_dereg_mode, NULL_TREE,
						add_when_allocated, caf_token);

	      gfc_add_expr_to_block (&tmpblock, tmp);
	    }
	  else if (attr->allocatable && !attr->codimension
		   && !deallocate_called)
	    {
	      /* Case of recursive allocatable derived types.  */
	      tree is_allocated;
	      tree ubound;
	      tree cdesc;
	      stmtblock_t dealloc_block;

	      gfc_init_block (&dealloc_block);
	      if (add_when_allocated)
		gfc_add_expr_to_block (&dealloc_block, add_when_allocated);

	      /* Convert the component into a rank 1 descriptor type.  */
	      if (attr->dimension)
		{
		  tmp = gfc_get_element_type (TREE_TYPE (comp));
		  ubound = gfc_full_array_size (&dealloc_block, comp,
						c->ts.type == BT_CLASS
						? CLASS_DATA (c)->as->rank
						: c->as->rank);
		}
	      else
		{
		  tmp = TREE_TYPE (comp);
		  ubound = build_int_cst (gfc_array_index_type, 1);
		}

	      cdesc = gfc_get_array_type_bounds (tmp, 1, 0, &gfc_index_one_node,
						 &ubound, 1,
						 GFC_ARRAY_ALLOCATABLE, false);

	      cdesc = gfc_create_var (cdesc, "cdesc");
	      DECL_ARTIFICIAL (cdesc) = 1;

	      gfc_add_modify (&dealloc_block, gfc_conv_descriptor_dtype (cdesc),
			      gfc_get_dtype_rank_type (1, tmp));
	      gfc_conv_descriptor_lbound_set (&dealloc_block, cdesc,
					      gfc_index_zero_node,
					      gfc_index_one_node);
	      gfc_conv_descriptor_stride_set (&dealloc_block, cdesc,
					      gfc_index_zero_node,
					      gfc_index_one_node);
	      gfc_conv_descriptor_ubound_set (&dealloc_block, cdesc,
					      gfc_index_zero_node, ubound);

	      if (attr->dimension)
		comp = gfc_conv_descriptor_data_get (comp);

	      gfc_conv_descriptor_data_set (&dealloc_block, cdesc, comp);

	      /* Now call the deallocator.  */
	      vtab = gfc_find_vtab (&c->ts);
	      if (vtab->backend_decl == NULL)
		gfc_get_symbol_decl (vtab);
	      tmp = gfc_build_addr_expr (NULL_TREE, vtab->backend_decl);
	      dealloc_fndecl = gfc_vptr_deallocate_get (tmp);
	      dealloc_fndecl = build_fold_indirect_ref_loc (input_location,
							    dealloc_fndecl);
	      tmp = build_int_cst (TREE_TYPE (comp), 0);
	      is_allocated = fold_build2_loc (input_location, NE_EXPR,
					      logical_type_node, tmp,
					      comp);
	      cdesc = gfc_build_addr_expr (NULL_TREE, cdesc);

	      tmp = build_call_expr_loc (input_location,
					 dealloc_fndecl, 1,
					 cdesc);
	      gfc_add_expr_to_block (&dealloc_block, tmp);

	      tmp = gfc_finish_block (&dealloc_block);

	      tmp = fold_build3_loc (input_location, COND_EXPR,
				     void_type_node, is_allocated, tmp,
				     build_empty_stmt (input_location));

	      gfc_add_expr_to_block (&tmpblock, tmp);
	    }
	  else if (add_when_allocated)
	    gfc_add_expr_to_block (&tmpblock, add_when_allocated);

	  if (c->ts.type == BT_CLASS && attr->allocatable
	      && (!attr->codimension || !caf_enabled (caf_mode)))
	    {
	      /* Finally, reset the vptr to the declared type vtable and, if
		 necessary reset the _len field.

		 First recover the reference to the component and obtain
		 the vptr.  */
	      comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				      decl, cdecl, NULL_TREE);
	      tmp = gfc_class_vptr_get (comp);

	      if (UNLIMITED_POLY (c))
		{
		  /* Both vptr and _len field should be nulled.  */
		  gfc_add_modify (&tmpblock, tmp,
				  build_int_cst (TREE_TYPE (tmp), 0));
		  tmp = gfc_class_len_get (comp);
		  gfc_add_modify (&tmpblock, tmp,
				  build_int_cst (TREE_TYPE (tmp), 0));
		}
	      else
		{
		  /* Build the vtable address and set the vptr with it.  */
		  tree vtab;
		  gfc_symbol *vtable;
		  vtable = gfc_find_derived_vtab (c->ts.u.derived);
		  vtab = vtable->backend_decl;
		  if (vtab == NULL_TREE)
		    vtab = gfc_get_symbol_decl (vtable);
		  vtab = gfc_build_addr_expr (NULL, vtab);
		  vtab = fold_convert (TREE_TYPE (tmp), vtab);
		  gfc_add_modify (&tmpblock, tmp, vtab);
		}
	    }

	  /* Now add the deallocation of this component.  */
	  gfc_add_block_to_block (&fnblock, &tmpblock);
	  break;

	case NULLIFY_ALLOC_COMP:
	  /* Nullify
	     - allocatable components (regular or in class)
	     - components that have allocatable components
	     - pointer components when in a coarray.
	     Skip everything else especially proc_pointers, which may come
	     coupled with the regular pointer attribute.  */
	  if (c->attr.proc_pointer
	      || !(c->attr.allocatable || (c->ts.type == BT_CLASS
					   && CLASS_DATA (c)->attr.allocatable)
		   || (cmp_has_alloc_comps
		       && ((c->ts.type == BT_DERIVED && !c->attr.pointer)
			   || (c->ts.type == BT_CLASS
			       && !CLASS_DATA (c)->attr.class_pointer)))
		   || (caf_in_coarray (caf_mode) && c->attr.pointer)))
	    continue;

	  /* Process class components first, because they always have the
	     pointer-attribute set which would be caught wrong else.  */
	  if (c->ts.type == BT_CLASS
	      && (CLASS_DATA (c)->attr.allocatable
		  || CLASS_DATA (c)->attr.class_pointer))
	    {
	      tree vptr_decl;

	      /* Allocatable CLASS components.  */
	      comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				      decl, cdecl, NULL_TREE);

	      vptr_decl = gfc_class_vptr_get (comp);

	      comp = gfc_class_data_get (comp);
	      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (comp)))
		gfc_conv_descriptor_data_set (&fnblock, comp,
					      null_pointer_node);
	      else
		{
		  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
					 void_type_node, comp,
					 build_int_cst (TREE_TYPE (comp), 0));
		  gfc_add_expr_to_block (&fnblock, tmp);
		}

	      /* The dynamic type of a disassociated pointer or unallocated
		 allocatable variable is its declared type. An unlimited
		 polymorphic entity has no declared type.  */
	      if (!UNLIMITED_POLY (c))
		{
		  vtab = gfc_find_derived_vtab (c->ts.u.derived);
		  if (!vtab->backend_decl)
		     gfc_get_symbol_decl (vtab);
		  tmp = gfc_build_addr_expr (NULL_TREE, vtab->backend_decl);
		}
	      else
		tmp = build_int_cst (TREE_TYPE (vptr_decl), 0);

	      tmp = fold_build2_loc (input_location, MODIFY_EXPR,
					 void_type_node, vptr_decl, tmp);
	      gfc_add_expr_to_block (&fnblock, tmp);

	      cmp_has_alloc_comps = false;
	    }
	  /* Coarrays need the component to be nulled before the api-call
	     is made.  */
	  else if (c->attr.pointer || c->attr.allocatable)
	    {
	      comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				      decl, cdecl, NULL_TREE);
	      if (c->attr.dimension || c->attr.codimension)
		gfc_conv_descriptor_data_set (&fnblock, comp,
					      null_pointer_node);
	      else
		gfc_add_modify (&fnblock, comp,
				build_int_cst (TREE_TYPE (comp), 0));
	      if (gfc_deferred_strlen (c, &comp))
		{
		  comp = fold_build3_loc (input_location, COMPONENT_REF,
					  TREE_TYPE (comp),
					  decl, comp, NULL_TREE);
		  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
					 TREE_TYPE (comp), comp,
					 build_int_cst (TREE_TYPE (comp), 0));
		  gfc_add_expr_to_block (&fnblock, tmp);
		}
	      cmp_has_alloc_comps = false;
	    }

	  if (flag_coarray == GFC_FCOARRAY_LIB && caf_in_coarray (caf_mode))
	    {
	      /* Register a component of a derived type coarray with the
		 coarray library.  Do not register ultimate component
		 coarrays here.  They are treated like regular coarrays and
		 are either allocated on all images or on none.  */
	      tree token;

	      comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				      decl, cdecl, NULL_TREE);
	      if (c->attr.dimension)
		{
		  /* Set the dtype, because caf_register needs it.  */
		  gfc_add_modify (&fnblock, gfc_conv_descriptor_dtype (comp),
				  gfc_get_dtype (TREE_TYPE (comp)));
		  tmp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
					 decl, cdecl, NULL_TREE);
		  token = gfc_conv_descriptor_token (tmp);
		}
	      else
		{
		  gfc_se se;

		  gfc_init_se (&se, NULL);
		  token = fold_build3_loc (input_location, COMPONENT_REF,
					   pvoid_type_node, decl, c->caf_token,
					   NULL_TREE);
		  comp = gfc_conv_scalar_to_descriptor (&se, comp,
							c->ts.type == BT_CLASS
							? CLASS_DATA (c)->attr
							: c->attr);
		  gfc_add_block_to_block (&fnblock, &se.pre);
		}

	      gfc_allocate_using_caf_lib (&fnblock, comp, size_zero_node,
					  gfc_build_addr_expr (NULL_TREE,
							       token),
					  NULL_TREE, NULL_TREE, NULL_TREE,
					  GFC_CAF_COARRAY_ALLOC_REGISTER_ONLY);
	    }

	  if (cmp_has_alloc_comps)
	    {
	      comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				      decl, cdecl, NULL_TREE);
	      rank = c->as ? c->as->rank : 0;
	      tmp = structure_alloc_comps (c->ts.u.derived, comp, NULL_TREE,
					   rank, purpose, caf_mode, args,
					   no_finalization);
	      gfc_add_expr_to_block (&fnblock, tmp);
	    }
	  break;

	case REASSIGN_CAF_COMP:
	  if (caf_enabled (caf_mode)
	      && (c->attr.codimension
		  || (c->ts.type == BT_CLASS
		      && (CLASS_DATA (c)->attr.coarray_comp
			  || caf_in_coarray (caf_mode)))
		  || (c->ts.type == BT_DERIVED
		      && (c->ts.u.derived->attr.coarray_comp
			  || caf_in_coarray (caf_mode))))
	      && !same_type)
	    {
	      comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				      decl, cdecl, NULL_TREE);
	      dcmp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				      dest, cdecl, NULL_TREE);

	      if (c->attr.codimension)
		{
		  if (c->ts.type == BT_CLASS)
		    {
		      comp = gfc_class_data_get (comp);
		      dcmp = gfc_class_data_get (dcmp);
		    }
		  gfc_conv_descriptor_data_set (&fnblock, dcmp,
					   gfc_conv_descriptor_data_get (comp));
		}
	      else
		{
		  tmp = structure_alloc_comps (c->ts.u.derived, comp, dcmp,
					       rank, purpose, caf_mode
					       | GFC_STRUCTURE_CAF_MODE_IN_COARRAY,
					       args, no_finalization);
		  gfc_add_expr_to_block (&fnblock, tmp);
		}
	    }
	  break;

	case COPY_ALLOC_COMP:
	  if (c->attr.pointer || c->attr.proc_pointer)
	    continue;

	  /* We need source and destination components.  */
	  comp = fold_build3_loc (input_location, COMPONENT_REF, ctype, decl,
				  cdecl, NULL_TREE);
	  dcmp = fold_build3_loc (input_location, COMPONENT_REF, ctype, dest,
				  cdecl, NULL_TREE);
	  dcmp = fold_convert (TREE_TYPE (comp), dcmp);

	  if (c->ts.type == BT_CLASS && CLASS_DATA (c)->attr.allocatable)
	    {
	      tree ftn_tree;
	      tree size;
	      tree dst_data;
	      tree src_data;
	      tree null_data;

	      dst_data = gfc_class_data_get (dcmp);
	      src_data = gfc_class_data_get (comp);
	      size = fold_convert (size_type_node,
				   gfc_class_vtab_size_get (comp));

	      if (CLASS_DATA (c)->attr.dimension)
		{
		  nelems = gfc_conv_descriptor_size (src_data,
						     CLASS_DATA (c)->as->rank);
		  size = fold_build2_loc (input_location, MULT_EXPR,
					  size_type_node, size,
					  fold_convert (size_type_node,
							nelems));
		}
	      else
		nelems = build_int_cst (size_type_node, 1);

	      if (CLASS_DATA (c)->attr.dimension
		  || CLASS_DATA (c)->attr.codimension)
		{
		  src_data = gfc_conv_descriptor_data_get (src_data);
		  dst_data = gfc_conv_descriptor_data_get (dst_data);
		}

	      gfc_init_block (&tmpblock);

	      gfc_add_modify (&tmpblock, gfc_class_vptr_get (dcmp),
			      gfc_class_vptr_get (comp));

	      /* Copy the unlimited '_len' field. If it is greater than zero
		 (ie. a character(_len)), multiply it by size and use this
		 for the malloc call.  */
	      if (UNLIMITED_POLY (c))
		{
		  gfc_add_modify (&tmpblock, gfc_class_len_get (dcmp),
				  gfc_class_len_get (comp));
		  size = gfc_resize_class_size_with_len (&tmpblock, comp, size);
		}

	      /* Coarray component have to have the same allocation status and
		 shape/type-parameter/effective-type on the LHS and RHS of an
		 intrinsic assignment. Hence, we did not deallocated them - and
		 do not allocate them here.  */
	      if (!CLASS_DATA (c)->attr.codimension)
		{
		  ftn_tree = builtin_decl_explicit (BUILT_IN_MALLOC);
		  tmp = build_call_expr_loc (input_location, ftn_tree, 1, size);
		  gfc_add_modify (&tmpblock, dst_data,
				  fold_convert (TREE_TYPE (dst_data), tmp));
		}

	      tmp = gfc_copy_class_to_class (comp, dcmp, nelems,
					     UNLIMITED_POLY (c));
	      gfc_add_expr_to_block (&tmpblock, tmp);
	      tmp = gfc_finish_block (&tmpblock);

	      gfc_init_block (&tmpblock);
	      gfc_add_modify (&tmpblock, dst_data,
			      fold_convert (TREE_TYPE (dst_data),
					    null_pointer_node));
	      null_data = gfc_finish_block (&tmpblock);

	      null_cond = fold_build2_loc (input_location, NE_EXPR,
					   logical_type_node, src_data,
				           null_pointer_node);

	      gfc_add_expr_to_block (&fnblock, build3_v (COND_EXPR, null_cond,
							 tmp, null_data));
	      continue;
	    }

	  /* To implement guarded deep copy, i.e., deep copy only allocatable
	     components that are really allocated, the deep copy code has to
	     be generated first and then added to the if-block in
	     gfc_duplicate_allocatable ().  */
	  if (cmp_has_alloc_comps && !c->attr.proc_pointer && !same_type)
	    {
	      rank = c->as ? c->as->rank : 0;
	      tmp = fold_convert (TREE_TYPE (dcmp), comp);
	      gfc_add_modify (&fnblock, dcmp, tmp);
	      add_when_allocated = structure_alloc_comps (c->ts.u.derived,
							  comp, dcmp,
							  rank, purpose,
							  caf_mode, args,
							  no_finalization);
	    }
	  else
	    add_when_allocated = NULL_TREE;

	  if (gfc_deferred_strlen (c, &tmp))
	    {
	      tree len, size;
	      len = tmp;
	      tmp = fold_build3_loc (input_location, COMPONENT_REF,
				     TREE_TYPE (len),
				     decl, len, NULL_TREE);
	      len = fold_build3_loc (input_location, COMPONENT_REF,
				     TREE_TYPE (len),
				     dest, len, NULL_TREE);
	      tmp = fold_build2_loc (input_location, MODIFY_EXPR,
				     TREE_TYPE (len), len, tmp);
	      gfc_add_expr_to_block (&fnblock, tmp);
	      size = size_of_string_in_bytes (c->ts.kind, len);
	      /* This component cannot have allocatable components,
		 therefore add_when_allocated of duplicate_allocatable ()
		 is always NULL.  */
	      rank = c->as ? c->as->rank : 0;
	      tmp = duplicate_allocatable (dcmp, comp, ctype, rank,
					   false, false, size, NULL_TREE);
	      gfc_add_expr_to_block (&fnblock, tmp);
	    }
	  else if (c->attr.pdt_array)
	    {
	      tmp = duplicate_allocatable (dcmp, comp, ctype,
					   c->as ? c->as->rank : 0,
					   false, false, NULL_TREE, NULL_TREE);
	      gfc_add_expr_to_block (&fnblock, tmp);
	    }
	  else if ((c->attr.allocatable)
		    && !c->attr.proc_pointer && !same_type
		    && (!(cmp_has_alloc_comps && c->as) || c->attr.codimension
			|| caf_in_coarray (caf_mode)))
	    {
	      rank = c->as ? c->as->rank : 0;
	      if (c->attr.codimension)
		tmp = gfc_copy_allocatable_data (dcmp, comp, ctype, rank);
	      else if (flag_coarray == GFC_FCOARRAY_LIB
		       && caf_in_coarray (caf_mode))
		{
		  tree dst_tok;
		  if (c->as)
		    dst_tok = gfc_conv_descriptor_token (dcmp);
		  else
		    {
		      /* For a scalar allocatable component the caf_token is
			 the next component.  */
		      if (!c->caf_token)
			  c->caf_token = c->next->backend_decl;
		      dst_tok = fold_build3_loc (input_location,
						 COMPONENT_REF,
						 pvoid_type_node, dest,
						 c->caf_token,
						 NULL_TREE);
		    }
		  tmp = duplicate_allocatable_coarray (dcmp, dst_tok, comp,
						       ctype, rank);
		}
	      else
		tmp = gfc_duplicate_allocatable (dcmp, comp, ctype, rank,
						 add_when_allocated);
	      gfc_add_expr_to_block (&fnblock, tmp);
	    }
	  else
	    if (cmp_has_alloc_comps || is_pdt_type)
	      gfc_add_expr_to_block (&fnblock, add_when_allocated);

	  break;

	case ALLOCATE_PDT_COMP:

	  comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				  decl, cdecl, NULL_TREE);

	  /* Set the PDT KIND and LEN fields.  */
	  if (c->attr.pdt_kind || c->attr.pdt_len)
	    {
	      gfc_se tse;
	      gfc_expr *c_expr = NULL;
	      gfc_actual_arglist *param = pdt_param_list;
	      gfc_init_se (&tse, NULL);
	      for (; param; param = param->next)
		if (param->name && !strcmp (c->name, param->name))
		  c_expr = param->expr;

	      if (!c_expr)
		c_expr = c->initializer;

	      if (c_expr)
		{
		  gfc_conv_expr_type (&tse, c_expr, TREE_TYPE (comp));
		  gfc_add_modify (&fnblock, comp, tse.expr);
		}
	    }

	  if (c->attr.pdt_string)
	    {
	      gfc_se tse;
	      gfc_init_se (&tse, NULL);
	      tree strlen = NULL_TREE;
	      gfc_expr *e = gfc_copy_expr (c->ts.u.cl->length);
	      /* Convert the parameterized string length to its value. The
		 string length is stored in a hidden field in the same way as
		 deferred string lengths.  */
	      gfc_insert_parameter_exprs (e, pdt_param_list);
	      if (gfc_deferred_strlen (c, &strlen) && strlen != NULL_TREE)
		{
		  gfc_conv_expr_type (&tse, e,
				      TREE_TYPE (strlen));
		  strlen = fold_build3_loc (input_location, COMPONENT_REF,
					    TREE_TYPE (strlen),
					    decl, strlen, NULL_TREE);
		  gfc_add_modify (&fnblock, strlen, tse.expr);
		  c->ts.u.cl->backend_decl = strlen;
		}
	      gfc_free_expr (e);

	      /* Scalar parameterized strings can be allocated now.  */
	      if (!c->as)
		{
		  tmp = fold_convert (gfc_array_index_type, strlen);
		  tmp = size_of_string_in_bytes (c->ts.kind, tmp);
		  tmp = gfc_evaluate_now (tmp, &fnblock);
		  tmp = gfc_call_malloc (&fnblock, TREE_TYPE (comp), tmp);
		  gfc_add_modify (&fnblock, comp, tmp);
		}
	    }

	  /* Allocate parameterized arrays of parameterized derived types.  */
	  if (!(c->attr.pdt_array && c->as && c->as->type == AS_EXPLICIT)
	      && !((c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
		   && (c->ts.u.derived && c->ts.u.derived->attr.pdt_type)))
	    continue;

	  if (c->ts.type == BT_CLASS)
	    comp = gfc_class_data_get (comp);

	  if (c->attr.pdt_array)
	    {
	      gfc_se tse;
	      int i;
	      tree size = gfc_index_one_node;
	      tree offset = gfc_index_zero_node;
	      tree lower, upper;
	      gfc_expr *e;

	      /* This chunk takes the expressions for 'lower' and 'upper'
		 in the arrayspec and substitutes in the expressions for
		 the parameters from 'pdt_param_list'. The descriptor
		 fields can then be filled from the values so obtained.  */
	      gcc_assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (comp)));
	      for (i = 0; i < c->as->rank; i++)
		{
		  gfc_init_se (&tse, NULL);
		  e = gfc_copy_expr (c->as->lower[i]);
		  gfc_insert_parameter_exprs (e, pdt_param_list);
		  gfc_conv_expr_type (&tse, e, gfc_array_index_type);
		  gfc_free_expr (e);
		  lower = tse.expr;
		  gfc_conv_descriptor_lbound_set (&fnblock, comp,
						  gfc_rank_cst[i],
						  lower);
		  e = gfc_copy_expr (c->as->upper[i]);
		  gfc_insert_parameter_exprs (e, pdt_param_list);
		  gfc_conv_expr_type (&tse, e, gfc_array_index_type);
		  gfc_free_expr (e);
		  upper = tse.expr;
		  gfc_conv_descriptor_ubound_set (&fnblock, comp,
						  gfc_rank_cst[i],
						  upper);
		  gfc_conv_descriptor_stride_set (&fnblock, comp,
						  gfc_rank_cst[i],
						  size);
		  size = gfc_evaluate_now (size, &fnblock);
		  offset = fold_build2_loc (input_location,
					    MINUS_EXPR,
					    gfc_array_index_type,
					    offset, size);
		  offset = gfc_evaluate_now (offset, &fnblock);
		  tmp = fold_build2_loc (input_location, MINUS_EXPR,
					 gfc_array_index_type,
					 upper, lower);
		  tmp = fold_build2_loc (input_location, PLUS_EXPR,
					 gfc_array_index_type,
					 tmp, gfc_index_one_node);
		  size = fold_build2_loc (input_location, MULT_EXPR,
					  gfc_array_index_type, size, tmp);
		}
	      gfc_conv_descriptor_offset_set (&fnblock, comp, offset);
	      if (c->ts.type == BT_CLASS)
		{
		  tmp = gfc_get_vptr_from_expr (comp);
		  if (POINTER_TYPE_P (TREE_TYPE (tmp)))
		    tmp = build_fold_indirect_ref_loc (input_location, tmp);
		  tmp = gfc_vptr_size_get (tmp);
		}
	      else
		tmp = TYPE_SIZE_UNIT (gfc_get_element_type (ctype));
	      tmp = fold_convert (gfc_array_index_type, tmp);
	      size = fold_build2_loc (input_location, MULT_EXPR,
				      gfc_array_index_type, size, tmp);
	      size = gfc_evaluate_now (size, &fnblock);
	      tmp = gfc_call_malloc (&fnblock, NULL, size);
	      gfc_conv_descriptor_data_set (&fnblock, comp, tmp);
	      tmp = gfc_conv_descriptor_dtype (comp);
	      gfc_add_modify (&fnblock, tmp, gfc_get_dtype (ctype));

	      if (c->initializer && c->initializer->rank)
		{
		  gfc_init_se (&tse, NULL);
		  e = gfc_copy_expr (c->initializer);
		  gfc_insert_parameter_exprs (e, pdt_param_list);
		  gfc_conv_expr_descriptor (&tse, e);
		  gfc_add_block_to_block (&fnblock, &tse.pre);
		  gfc_free_expr (e);
		  tmp = builtin_decl_explicit (BUILT_IN_MEMCPY);
		  tmp = build_call_expr_loc (input_location, tmp, 3,
				     gfc_conv_descriptor_data_get (comp),
				     gfc_conv_descriptor_data_get (tse.expr),
				     fold_convert (size_type_node, size));
		  gfc_add_expr_to_block (&fnblock, tmp);
		  gfc_add_block_to_block (&fnblock, &tse.post);
		}
	    }

	  /* Recurse in to PDT components.  */
	  if ((c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
	      && c->ts.u.derived && c->ts.u.derived->attr.pdt_type
	      && !(c->attr.pointer || c->attr.allocatable))
	    {
	      bool is_deferred = false;
	      gfc_actual_arglist *tail = c->param_list;

	      for (; tail; tail = tail->next)
		if (!tail->expr)
		  is_deferred = true;

	      tail = is_deferred ? pdt_param_list : c->param_list;
	      tmp = gfc_allocate_pdt_comp (c->ts.u.derived, comp,
					   c->as ? c->as->rank : 0,
					   tail);
	      gfc_add_expr_to_block (&fnblock, tmp);
	    }

	  break;

	case DEALLOCATE_PDT_COMP:
	  /* Deallocate array or parameterized string length components
	     of parameterized derived types.  */
	  if (!(c->attr.pdt_array && c->as && c->as->type == AS_EXPLICIT)
	      && !c->attr.pdt_string
	      && !((c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
		   && (c->ts.u.derived && c->ts.u.derived->attr.pdt_type)))
	    continue;

	  comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				  decl, cdecl, NULL_TREE);
	  if (c->ts.type == BT_CLASS)
	    comp = gfc_class_data_get (comp);

	  /* Recurse in to PDT components.  */
	  if ((c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
	      && c->ts.u.derived && c->ts.u.derived->attr.pdt_type
	      && (!c->attr.pointer && !c->attr.allocatable))
	    {
	      tmp = gfc_deallocate_pdt_comp (c->ts.u.derived, comp,
					     c->as ? c->as->rank : 0);
	      gfc_add_expr_to_block (&fnblock, tmp);
	    }

	  if (c->attr.pdt_array || c->attr.pdt_string)
	    {
	      tmp = comp;
	      if (c->attr.pdt_array)
		tmp = gfc_conv_descriptor_data_get (comp);
	      null_cond = fold_build2_loc (input_location, NE_EXPR,
					   logical_type_node, tmp,
					   build_int_cst (TREE_TYPE (tmp), 0));
	      if (flag_openmp_allocators)
		{
		  tree cd, t;
		  if (c->attr.pdt_array)
		    cd = fold_build2_loc (input_location, EQ_EXPR,
					  boolean_type_node,
					  gfc_conv_descriptor_version (comp),
					  build_int_cst (integer_type_node, 1));
		  else
		    cd = gfc_omp_call_is_alloc (tmp);
		  t = builtin_decl_explicit (BUILT_IN_GOMP_FREE);
		  t = build_call_expr_loc (input_location, t, 1, tmp);

		  stmtblock_t tblock;
		  gfc_init_block (&tblock);
		  gfc_add_expr_to_block (&tblock, t);
		  if (c->attr.pdt_array)
		    gfc_add_modify (&tblock, gfc_conv_descriptor_version (comp),
				    integer_zero_node);
		  tmp = build3_loc (input_location, COND_EXPR, void_type_node,
				    cd, gfc_finish_block (&tblock),
				    gfc_call_free (tmp));
		}
	      else
		tmp = gfc_call_free (tmp);
	      tmp = build3_v (COND_EXPR, null_cond, tmp,
			      build_empty_stmt (input_location));
	      gfc_add_expr_to_block (&fnblock, tmp);

	      if (c->attr.pdt_array)
		gfc_conv_descriptor_data_set (&fnblock, comp, null_pointer_node);
	      else
		{
		  tmp = fold_convert (TREE_TYPE (comp), null_pointer_node);
		  gfc_add_modify (&fnblock, comp, tmp);
		}
	    }

	  break;

	case CHECK_PDT_DUMMY:

	  comp = fold_build3_loc (input_location, COMPONENT_REF, ctype,
				  decl, cdecl, NULL_TREE);
	  if (c->ts.type == BT_CLASS)
	    comp = gfc_class_data_get (comp);

	  /* Recurse in to PDT components.  */
	  if ((c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
	      && c->ts.u.derived && c->ts.u.derived->attr.pdt_type)
	    {
	      tmp = gfc_check_pdt_dummy (c->ts.u.derived, comp,
					 c->as ? c->as->rank : 0,
					 pdt_param_list);
	      gfc_add_expr_to_block (&fnblock, tmp);
	    }

	  if (!c->attr.pdt_len)
	    continue;
	  else
	    {
	      gfc_se tse;
	      gfc_expr *c_expr = NULL;
	      gfc_actual_arglist *param = pdt_param_list;

	      gfc_init_se (&tse, NULL);
	      for (; param; param = param->next)
		if (!strcmp (c->name, param->name)
		    && param->spec_type == SPEC_EXPLICIT)
		  c_expr = param->expr;

	      if (c_expr)
		{
		  tree error, cond, cname;
		  gfc_conv_expr_type (&tse, c_expr, TREE_TYPE (comp));
		  cond = fold_build2_loc (input_location, NE_EXPR,
					  logical_type_node,
					  comp, tse.expr);
		  cname = gfc_build_cstring_const (c->name);
		  cname = gfc_build_addr_expr (pchar_type_node, cname);
		  error = gfc_trans_runtime_error (true, NULL,
						   "The value of the PDT LEN "
						   "parameter '%s' does not "
						   "agree with that in the "
						   "dummy declaration",
						   cname);
		  tmp = fold_build3_loc (input_location, COND_EXPR,
					 void_type_node, cond, error,
					 build_empty_stmt (input_location));
		  gfc_add_expr_to_block (&fnblock, tmp);
		}
	    }
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
    }

  return gfc_finish_block (&fnblock);
}

/* Recursively traverse an object of derived type, generating code to
   nullify allocatable components.  */

tree
gfc_nullify_alloc_comp (gfc_symbol * der_type, tree decl, int rank,
			int caf_mode)
{
  return structure_alloc_comps (der_type, decl, NULL_TREE, rank,
				NULLIFY_ALLOC_COMP,
				GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY | caf_mode,
				NULL);
}


/* Recursively traverse an object of derived type, generating code to
   deallocate allocatable components.  */

tree
gfc_deallocate_alloc_comp (gfc_symbol * der_type, tree decl, int rank,
			   int caf_mode)
{
  return structure_alloc_comps (der_type, decl, NULL_TREE, rank,
				DEALLOCATE_ALLOC_COMP,
				GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY | caf_mode,
				NULL);
}

tree
gfc_bcast_alloc_comp (gfc_symbol *derived, gfc_expr *expr, int rank,
		      tree image_index, tree stat, tree errmsg,
		      tree errmsg_len)
{
  tree tmp, array;
  gfc_se argse;
  stmtblock_t block, post_block;
  gfc_co_subroutines_args args;

  args.image_index = image_index;
  args.stat = stat;
  args.errmsg = errmsg;
  args.errmsg_len = errmsg_len;

  if (rank == 0)
    {
      gfc_start_block (&block);
      gfc_init_block (&post_block);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr (&argse, expr);
      gfc_add_block_to_block (&block, &argse.pre);
      gfc_add_block_to_block (&post_block, &argse.post);
      array = argse.expr;
    }
  else
    {
      gfc_init_se (&argse, NULL);
      argse.want_pointer = 1;
      gfc_conv_expr_descriptor (&argse, expr);
      array = argse.expr;
    }

  tmp = structure_alloc_comps (derived, array, NULL_TREE, rank,
			       BCAST_ALLOC_COMP,
			       GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY,
			       &args);
  return tmp;
}

/* Recursively traverse an object of derived type, generating code to
   deallocate allocatable components.  But do not deallocate coarrays.
   To be used for intrinsic assignment, which may not change the allocation
   status of coarrays.  */

tree
gfc_deallocate_alloc_comp_no_caf (gfc_symbol * der_type, tree decl, int rank,
				  bool no_finalization)
{
  return structure_alloc_comps (der_type, decl, NULL_TREE, rank,
				DEALLOCATE_ALLOC_COMP, 0, NULL,
				no_finalization);
}


tree
gfc_reassign_alloc_comp_caf (gfc_symbol *der_type, tree decl, tree dest)
{
  return structure_alloc_comps (der_type, decl, dest, 0, REASSIGN_CAF_COMP,
				GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY,
				NULL);
}


/* Recursively traverse an object of derived type, generating code to
   copy it and its allocatable components.  */

tree
gfc_copy_alloc_comp (gfc_symbol * der_type, tree decl, tree dest, int rank,
		     int caf_mode)
{
  return structure_alloc_comps (der_type, decl, dest, rank, COPY_ALLOC_COMP,
				caf_mode, NULL);
}


/* Recursively traverse an object of derived type, generating code to
   copy it and its allocatable components, while suppressing any
   finalization that might occur.  This is used in the finalization of
   function results.  */

tree
gfc_copy_alloc_comp_no_fini (gfc_symbol * der_type, tree decl, tree dest,
			     int rank, int caf_mode)
{
  return structure_alloc_comps (der_type, decl, dest, rank, COPY_ALLOC_COMP,
				caf_mode, NULL, true);
}


/* Recursively traverse an object of derived type, generating code to
   copy only its allocatable components.  */

tree
gfc_copy_only_alloc_comp (gfc_symbol * der_type, tree decl, tree dest, int rank)
{
  return structure_alloc_comps (der_type, decl, dest, rank,
				COPY_ONLY_ALLOC_COMP, 0, NULL);
}


/* Recursively traverse an object of parameterized derived type, generating
   code to allocate parameterized components.  */

tree
gfc_allocate_pdt_comp (gfc_symbol * der_type, tree decl, int rank,
		       gfc_actual_arglist *param_list)
{
  tree res;
  gfc_actual_arglist *old_param_list = pdt_param_list;
  pdt_param_list = param_list;
  res = structure_alloc_comps (der_type, decl, NULL_TREE, rank,
			       ALLOCATE_PDT_COMP, 0, NULL);
  pdt_param_list = old_param_list;
  return res;
}

/* Recursively traverse an object of parameterized derived type, generating
   code to deallocate parameterized components.  */

tree
gfc_deallocate_pdt_comp (gfc_symbol * der_type, tree decl, int rank)
{
  return structure_alloc_comps (der_type, decl, NULL_TREE, rank,
				DEALLOCATE_PDT_COMP, 0, NULL);
}


/* Recursively traverse a dummy of parameterized derived type to check the
   values of LEN parameters.  */

tree
gfc_check_pdt_dummy (gfc_symbol * der_type, tree decl, int rank,
		     gfc_actual_arglist *param_list)
{
  tree res;
  gfc_actual_arglist *old_param_list = pdt_param_list;
  pdt_param_list = param_list;
  res = structure_alloc_comps (der_type, decl, NULL_TREE, rank,
			       CHECK_PDT_DUMMY, 0, NULL);
  pdt_param_list = old_param_list;
  return res;
}


/* Returns the value of LBOUND for an expression.  This could be broken out
   from gfc_conv_intrinsic_bound but this seemed to be simpler.  This is
   called by gfc_alloc_allocatable_for_assignment.  */
static tree
get_std_lbound (gfc_expr *expr, tree desc, int dim, bool assumed_size)
{
  tree lbound;
  tree ubound;
  tree stride;
  tree cond, cond1, cond3, cond4;
  tree tmp;
  gfc_ref *ref;

  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))
    {
      tmp = gfc_rank_cst[dim];
      lbound = gfc_conv_descriptor_lbound_get (desc, tmp);
      ubound = gfc_conv_descriptor_ubound_get (desc, tmp);
      stride = gfc_conv_descriptor_stride_get (desc, tmp);
      cond1 = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
			       ubound, lbound);
      cond3 = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
			       stride, gfc_index_zero_node);
      cond3 = fold_build2_loc (input_location, TRUTH_AND_EXPR,
			       logical_type_node, cond3, cond1);
      cond4 = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
			       stride, gfc_index_zero_node);
      if (assumed_size)
	cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				tmp, build_int_cst (gfc_array_index_type,
						    expr->rank - 1));
      else
	cond = logical_false_node;

      cond1 = fold_build2_loc (input_location, TRUTH_OR_EXPR,
			       logical_type_node, cond3, cond4);
      cond = fold_build2_loc (input_location, TRUTH_OR_EXPR,
			      logical_type_node, cond, cond1);

      return fold_build3_loc (input_location, COND_EXPR,
			      gfc_array_index_type, cond,
			      lbound, gfc_index_one_node);
    }

  if (expr->expr_type == EXPR_FUNCTION)
    {
      /* A conversion function, so use the argument.  */
      gcc_assert (expr->value.function.isym
		  && expr->value.function.isym->conversion);
      expr = expr->value.function.actual->expr;
    }

  if (expr->expr_type == EXPR_VARIABLE)
    {
      tmp = TREE_TYPE (expr->symtree->n.sym->backend_decl);
      for (ref = expr->ref; ref; ref = ref->next)
	{
	  if (ref->type == REF_COMPONENT
		&& ref->u.c.component->as
		&& ref->next
		&& ref->next->u.ar.type == AR_FULL)
	    tmp = TREE_TYPE (ref->u.c.component->backend_decl);
	}
      return GFC_TYPE_ARRAY_LBOUND(tmp, dim);
    }

  return gfc_index_one_node;
}


/* Returns true if an expression represents an lhs that can be reallocated
   on assignment.  */

bool
gfc_is_reallocatable_lhs (gfc_expr *expr)
{
  gfc_ref * ref;
  gfc_symbol *sym;

  if (!expr->ref)
    return false;

  sym = expr->symtree->n.sym;

  if (sym->attr.associate_var && !expr->ref)
    return false;

  /* An allocatable class variable with no reference.  */
  if (sym->ts.type == BT_CLASS
      && (!sym->attr.associate_var || sym->attr.select_rank_temporary)
      && CLASS_DATA (sym)->attr.allocatable
      && expr->ref
      && ((expr->ref->type == REF_ARRAY && expr->ref->u.ar.type == AR_FULL
	   && expr->ref->next == NULL)
	  || (expr->ref->type == REF_COMPONENT
	      && strcmp (expr->ref->u.c.component->name, "_data") == 0
	      && (expr->ref->next == NULL
		  || (expr->ref->next->type == REF_ARRAY
		      && expr->ref->next->u.ar.type == AR_FULL
		      && expr->ref->next->next == NULL)))))
    return true;

  /* An allocatable variable.  */
  if (sym->attr.allocatable
      && (!sym->attr.associate_var || sym->attr.select_rank_temporary)
      && expr->ref
      && expr->ref->type == REF_ARRAY
      && expr->ref->u.ar.type == AR_FULL)
    return true;

  /* All that can be left are allocatable components.  */
  if ((sym->ts.type != BT_DERIVED
       && sym->ts.type != BT_CLASS)
	|| !sym->ts.u.derived->attr.alloc_comp)
    return false;

  /* Find a component ref followed by an array reference.  */
  for (ref = expr->ref; ref; ref = ref->next)
    if (ref->next
	  && ref->type == REF_COMPONENT
	  && ref->next->type == REF_ARRAY
	  && !ref->next->next)
      break;

  if (!ref)
    return false;

  /* Return true if valid reallocatable lhs.  */
  if (ref->u.c.component->attr.allocatable
	&& ref->next->u.ar.type == AR_FULL)
    return true;

  return false;
}


static tree
concat_str_length (gfc_expr* expr)
{
  tree type;
  tree len1;
  tree len2;
  gfc_se se;

  type = gfc_typenode_for_spec (&expr->value.op.op1->ts);
  len1 = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
  if (len1 == NULL_TREE)
    {
      if (expr->value.op.op1->expr_type == EXPR_OP)
	len1 = concat_str_length (expr->value.op.op1);
      else if (expr->value.op.op1->expr_type == EXPR_CONSTANT)
	len1 = build_int_cst (gfc_charlen_type_node,
			expr->value.op.op1->value.character.length);
      else if (expr->value.op.op1->ts.u.cl->length)
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, expr->value.op.op1->ts.u.cl->length);
	  len1 = se.expr;
	}
      else
	{
	  /* Last resort!  */
	  gfc_init_se (&se, NULL);
	  se.want_pointer = 1;
	  se.descriptor_only = 1;
	  gfc_conv_expr (&se, expr->value.op.op1);
	  len1 = se.string_length;
	}
    }

  type = gfc_typenode_for_spec (&expr->value.op.op2->ts);
  len2 = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
  if (len2 == NULL_TREE)
    {
      if (expr->value.op.op2->expr_type == EXPR_OP)
	len2 = concat_str_length (expr->value.op.op2);
      else if (expr->value.op.op2->expr_type == EXPR_CONSTANT)
	len2 = build_int_cst (gfc_charlen_type_node,
			expr->value.op.op2->value.character.length);
      else if (expr->value.op.op2->ts.u.cl->length)
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, expr->value.op.op2->ts.u.cl->length);
	  len2 = se.expr;
	}
      else
	{
	  /* Last resort!  */
	  gfc_init_se (&se, NULL);
	  se.want_pointer = 1;
	  se.descriptor_only = 1;
	  gfc_conv_expr (&se, expr->value.op.op2);
	  len2 = se.string_length;
	}
    }

  gcc_assert(len1 && len2);
  len1 = fold_convert (gfc_charlen_type_node, len1);
  len2 = fold_convert (gfc_charlen_type_node, len2);

  return fold_build2_loc (input_location, PLUS_EXPR,
			  gfc_charlen_type_node, len1, len2);
}


/* Allocate the lhs of an assignment to an allocatable array, otherwise
   reallocate it.  */

tree
gfc_alloc_allocatable_for_assignment (gfc_loopinfo *loop,
				      gfc_expr *expr1,
				      gfc_expr *expr2)
{
  stmtblock_t realloc_block;
  stmtblock_t alloc_block;
  stmtblock_t fblock;
  gfc_ss *rss;
  gfc_ss *lss;
  gfc_array_info *linfo;
  tree realloc_expr;
  tree alloc_expr;
  tree size1;
  tree size2;
  tree elemsize1;
  tree elemsize2;
  tree array1;
  tree cond_null;
  tree cond;
  tree tmp;
  tree tmp2;
  tree lbound;
  tree ubound;
  tree desc;
  tree old_desc;
  tree desc2;
  tree offset;
  tree jump_label1;
  tree jump_label2;
  tree lbd;
  tree class_expr2 = NULL_TREE;
  int n;
  int dim;
  gfc_array_spec * as;
  bool coarray = (flag_coarray == GFC_FCOARRAY_LIB
		  && gfc_caf_attr (expr1, true).codimension);
  tree token;
  gfc_se caf_se;

  /* x = f(...) with x allocatable.  In this case, expr1 is the rhs.
     Find the lhs expression in the loop chain and set expr1 and
     expr2 accordingly.  */
  if (expr1->expr_type == EXPR_FUNCTION && expr2 == NULL)
    {
      expr2 = expr1;
      /* Find the ss for the lhs.  */
      lss = loop->ss;
      for (; lss && lss != gfc_ss_terminator; lss = lss->loop_chain)
	if (lss->info->expr && lss->info->expr->expr_type == EXPR_VARIABLE)
	  break;
      if (lss == gfc_ss_terminator)
	return NULL_TREE;
      expr1 = lss->info->expr;
    }

  /* Bail out if this is not a valid allocate on assignment.  */
  if (!gfc_is_reallocatable_lhs (expr1)
	|| (expr2 && !expr2->rank))
    return NULL_TREE;

  /* Find the ss for the lhs.  */
  lss = loop->ss;
  for (; lss && lss != gfc_ss_terminator; lss = lss->loop_chain)
    if (lss->info->expr == expr1)
      break;

  if (lss == gfc_ss_terminator)
    return NULL_TREE;

  linfo = &lss->info->data.array;

  /* Find an ss for the rhs. For operator expressions, we see the
     ss's for the operands. Any one of these will do.  */
  rss = loop->ss;
  for (; rss && rss != gfc_ss_terminator; rss = rss->loop_chain)
    if (rss->info->expr != expr1 && rss != loop->temp_ss)
      break;

  if (expr2 && rss == gfc_ss_terminator)
    return NULL_TREE;

  /* Ensure that the string length from the current scope is used.  */
  if (expr2->ts.type == BT_CHARACTER
      && expr2->expr_type == EXPR_FUNCTION
      && !expr2->value.function.isym)
    expr2->ts.u.cl->backend_decl = rss->info->string_length;

  gfc_start_block (&fblock);

  /* Since the lhs is allocatable, this must be a descriptor type.
     Get the data and array size.  */
  desc = linfo->descriptor;
  gcc_assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)));
  array1 = gfc_conv_descriptor_data_get (desc);

  if (expr2)
    desc2 = rss->info->data.array.descriptor;
  else
    desc2 = NULL_TREE;

  /* Get the old lhs element size for deferred character and class expr1.  */
  if (expr1->ts.type == BT_CHARACTER && expr1->ts.deferred)
    {
      if (expr1->ts.u.cl->backend_decl
	  && VAR_P (expr1->ts.u.cl->backend_decl))
	elemsize1 = expr1->ts.u.cl->backend_decl;
      else
	elemsize1 = lss->info->string_length;
      tree unit_size = TYPE_SIZE_UNIT (gfc_get_char_type (expr1->ts.kind));
      elemsize1 = fold_build2_loc (input_location, MULT_EXPR,
				   TREE_TYPE (elemsize1), elemsize1,
				   fold_convert (TREE_TYPE (elemsize1), unit_size));

    }
  else if (expr1->ts.type == BT_CLASS)
    {
      /* Unfortunately, the lhs vptr is set too early in many cases.
	 Play it safe by using the descriptor element length.  */
      tmp = gfc_conv_descriptor_elem_len (desc);
      elemsize1 = fold_convert (gfc_array_index_type, tmp);
    }
  else
    elemsize1 = NULL_TREE;
  if (elemsize1 != NULL_TREE)
    elemsize1 = gfc_evaluate_now (elemsize1, &fblock);

  /* Get the new lhs size in bytes.  */
  if (expr1->ts.type == BT_CHARACTER && expr1->ts.deferred)
    {
      if (expr2->ts.deferred)
	{
	  if (expr2->ts.u.cl->backend_decl
	      && VAR_P (expr2->ts.u.cl->backend_decl))
	    tmp = expr2->ts.u.cl->backend_decl;
	  else
	    tmp = rss->info->string_length;
	}
      else
	{
	  tmp = expr2->ts.u.cl->backend_decl;
	  if (!tmp && expr2->expr_type == EXPR_OP
	      && expr2->value.op.op == INTRINSIC_CONCAT)
	    {
	      tmp = concat_str_length (expr2);
	      expr2->ts.u.cl->backend_decl = gfc_evaluate_now (tmp, &fblock);
	    }
	  else if (!tmp && expr2->ts.u.cl->length)
	    {
	      gfc_se tmpse;
	      gfc_init_se (&tmpse, NULL);
	      gfc_conv_expr_type (&tmpse, expr2->ts.u.cl->length,
				  gfc_charlen_type_node);
	      tmp = tmpse.expr;
	      expr2->ts.u.cl->backend_decl = gfc_evaluate_now (tmp, &fblock);
	    }
	  tmp = fold_convert (TREE_TYPE (expr1->ts.u.cl->backend_decl), tmp);
	}

      if (expr1->ts.u.cl->backend_decl
	  && VAR_P (expr1->ts.u.cl->backend_decl))
	gfc_add_modify (&fblock, expr1->ts.u.cl->backend_decl, tmp);
      else
	gfc_add_modify (&fblock, lss->info->string_length, tmp);

      if (expr1->ts.kind > 1)
	tmp = fold_build2_loc (input_location, MULT_EXPR,
			       TREE_TYPE (tmp),
			       tmp, build_int_cst (TREE_TYPE (tmp),
						   expr1->ts.kind));
    }
  else if (expr1->ts.type == BT_CHARACTER && expr1->ts.u.cl->backend_decl)
    {
      tmp = TYPE_SIZE_UNIT (TREE_TYPE (gfc_typenode_for_spec (&expr1->ts)));
      tmp = fold_build2_loc (input_location, MULT_EXPR,
			     gfc_array_index_type, tmp,
			     expr1->ts.u.cl->backend_decl);
    }
  else if (UNLIMITED_POLY (expr1) && expr2->ts.type != BT_CLASS)
    tmp = TYPE_SIZE_UNIT (gfc_typenode_for_spec (&expr2->ts));
  else if (expr1->ts.type == BT_CLASS && expr2->ts.type == BT_CLASS)
    {
      tmp = expr2->rank ? gfc_get_class_from_expr (desc2) : NULL_TREE;
      if (tmp == NULL_TREE && expr2->expr_type == EXPR_VARIABLE)
	tmp = class_expr2 = gfc_get_class_from_gfc_expr (expr2);

      if (tmp != NULL_TREE)
	tmp = gfc_class_vtab_size_get (tmp);
      else
	tmp = TYPE_SIZE_UNIT (gfc_typenode_for_spec (&CLASS_DATA (expr2)->ts));
    }
  else
    tmp = TYPE_SIZE_UNIT (gfc_typenode_for_spec (&expr2->ts));
  elemsize2 = fold_convert (gfc_array_index_type, tmp);
  elemsize2 = gfc_evaluate_now (elemsize2, &fblock);

  /* 7.4.1.3 "If variable is an allocated allocatable variable, it is
     deallocated if expr is an array of different shape or any of the
     corresponding length type parameter values of variable and expr
     differ."  This assures F95 compatibility.  */
  jump_label1 = gfc_build_label_decl (NULL_TREE);
  jump_label2 = gfc_build_label_decl (NULL_TREE);

  /* Allocate if data is NULL.  */
  cond_null = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			 array1, build_int_cst (TREE_TYPE (array1), 0));
  cond_null= gfc_evaluate_now (cond_null, &fblock);

  tmp = build3_v (COND_EXPR, cond_null,
		  build1_v (GOTO_EXPR, jump_label1),
		  build_empty_stmt (input_location));
  gfc_add_expr_to_block (&fblock, tmp);

  /* Get arrayspec if expr is a full array.  */
  if (expr2 && expr2->expr_type == EXPR_FUNCTION
	&& expr2->value.function.isym
	&& expr2->value.function.isym->conversion)
    {
      /* For conversion functions, take the arg.  */
      gfc_expr *arg = expr2->value.function.actual->expr;
      as = gfc_get_full_arrayspec_from_expr (arg);
    }
  else if (expr2)
    as = gfc_get_full_arrayspec_from_expr (expr2);
  else
    as = NULL;

  /* If the lhs shape is not the same as the rhs jump to setting the
     bounds and doing the reallocation.......  */
  for (n = 0; n < expr1->rank; n++)
    {
      /* Check the shape.  */
      lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[n]);
      ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[n]);
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type,
			     loop->to[n], loop->from[n]);
      tmp = fold_build2_loc (input_location, PLUS_EXPR,
			     gfc_array_index_type,
			     tmp, lbound);
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type,
			     tmp, ubound);
      cond = fold_build2_loc (input_location, NE_EXPR,
			      logical_type_node,
			      tmp, gfc_index_zero_node);
      tmp = build3_v (COND_EXPR, cond,
		      build1_v (GOTO_EXPR, jump_label1),
		      build_empty_stmt (input_location));
      gfc_add_expr_to_block (&fblock, tmp);
    }

  /* ...else if the element lengths are not the same also go to
     setting the bounds and doing the reallocation.... */
  if (elemsize1 != NULL_TREE)
    {
      cond = fold_build2_loc (input_location, NE_EXPR,
			      logical_type_node,
			      elemsize1, elemsize2);
      tmp = build3_v (COND_EXPR, cond,
		      build1_v (GOTO_EXPR, jump_label1),
		      build_empty_stmt (input_location));
      gfc_add_expr_to_block (&fblock, tmp);
    }

  /* ....else jump past the (re)alloc code.  */
  tmp = build1_v (GOTO_EXPR, jump_label2);
  gfc_add_expr_to_block (&fblock, tmp);

  /* Add the label to start automatic (re)allocation.  */
  tmp = build1_v (LABEL_EXPR, jump_label1);
  gfc_add_expr_to_block (&fblock, tmp);

  /* Get the rhs size and fix it.  */
  size2 = gfc_index_one_node;
  for (n = 0; n < expr2->rank; n++)
    {
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type,
			     loop->to[n], loop->from[n]);
      tmp = fold_build2_loc (input_location, PLUS_EXPR,
			     gfc_array_index_type,
			     tmp, gfc_index_one_node);
      size2 = fold_build2_loc (input_location, MULT_EXPR,
			       gfc_array_index_type,
			       tmp, size2);
    }
  size2 = gfc_evaluate_now (size2, &fblock);

  /* Deallocation of allocatable components will have to occur on
     reallocation.  Fix the old descriptor now.  */
  if ((expr1->ts.type == BT_DERIVED)
	&& expr1->ts.u.derived->attr.alloc_comp)
    old_desc = gfc_evaluate_now (desc, &fblock);
  else
    old_desc = NULL_TREE;

  /* Now modify the lhs descriptor and the associated scalarizer
     variables. F2003 7.4.1.3: "If variable is or becomes an
     unallocated allocatable variable, then it is allocated with each
     deferred type parameter equal to the corresponding type parameters
     of expr , with the shape of expr , and with each lower bound equal
     to the corresponding element of LBOUND(expr)."
     Reuse size1 to keep a dimension-by-dimension track of the
     stride of the new array.  */
  size1 = gfc_index_one_node;
  offset = gfc_index_zero_node;

  for (n = 0; n < expr2->rank; n++)
    {
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type,
			     loop->to[n], loop->from[n]);
      tmp = fold_build2_loc (input_location, PLUS_EXPR,
			     gfc_array_index_type,
			     tmp, gfc_index_one_node);

      lbound = gfc_index_one_node;
      ubound = tmp;

      if (as)
	{
	  lbd = get_std_lbound (expr2, desc2, n,
				as->type == AS_ASSUMED_SIZE);
	  ubound = fold_build2_loc (input_location,
				    MINUS_EXPR,
				    gfc_array_index_type,
				    ubound, lbound);
	  ubound = fold_build2_loc (input_location,
				    PLUS_EXPR,
				    gfc_array_index_type,
				    ubound, lbd);
	  lbound = lbd;
	}

      gfc_conv_descriptor_lbound_set (&fblock, desc,
				      gfc_rank_cst[n],
				      lbound);
      gfc_conv_descriptor_ubound_set (&fblock, desc,
				      gfc_rank_cst[n],
				      ubound);
      gfc_conv_descriptor_stride_set (&fblock, desc,
				      gfc_rank_cst[n],
				      size1);
      lbound = gfc_conv_descriptor_lbound_get (desc,
					       gfc_rank_cst[n]);
      tmp2 = fold_build2_loc (input_location, MULT_EXPR,
			      gfc_array_index_type,
			      lbound, size1);
      offset = fold_build2_loc (input_location, MINUS_EXPR,
				gfc_array_index_type,
				offset, tmp2);
      size1 = fold_build2_loc (input_location, MULT_EXPR,
			       gfc_array_index_type,
			       tmp, size1);
    }

  /* Set the lhs descriptor and scalarizer offsets.  For rank > 1,
     the array offset is saved and the info.offset is used for a
     running offset.  Use the saved_offset instead.  */
  tmp = gfc_conv_descriptor_offset (desc);
  gfc_add_modify (&fblock, tmp, offset);
  if (linfo->saved_offset
      && VAR_P (linfo->saved_offset))
    gfc_add_modify (&fblock, linfo->saved_offset, tmp);

  /* Now set the deltas for the lhs.  */
  for (n = 0; n < expr1->rank; n++)
    {
      tmp = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[n]);
      dim = lss->dim[n];
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type, tmp,
			     loop->from[dim]);
      if (linfo->delta[dim] && VAR_P (linfo->delta[dim]))
	gfc_add_modify (&fblock, linfo->delta[dim], tmp);
    }

  /* Take into account _len of unlimited polymorphic entities, so that span
     for array descriptors and allocation sizes are computed correctly.  */
  if (UNLIMITED_POLY (expr2))
    {
      tree len = gfc_class_len_get (TREE_OPERAND (desc2, 0));
      len = fold_build2_loc (input_location, MAX_EXPR, size_type_node,
			     fold_convert (size_type_node, len),
			     size_one_node);
      elemsize2 = fold_build2_loc (input_location, MULT_EXPR,
				   gfc_array_index_type, elemsize2,
				   fold_convert (gfc_array_index_type, len));
    }

  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))
    gfc_conv_descriptor_span_set (&fblock, desc, elemsize2);

  size2 = fold_build2_loc (input_location, MULT_EXPR,
			   gfc_array_index_type,
			   elemsize2, size2);
  size2 = fold_convert (size_type_node, size2);
  size2 = fold_build2_loc (input_location, MAX_EXPR, size_type_node,
			   size2, size_one_node);
  size2 = gfc_evaluate_now (size2, &fblock);

  /* For deferred character length, the 'size' field of the dtype might
     have changed so set the dtype.  */
  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc))
      && expr1->ts.type == BT_CHARACTER && expr1->ts.deferred)
    {
      tree type;
      tmp = gfc_conv_descriptor_dtype (desc);
      if (expr2->ts.u.cl->backend_decl)
	type = gfc_typenode_for_spec (&expr2->ts);
      else
	type = gfc_typenode_for_spec (&expr1->ts);

      gfc_add_modify (&fblock, tmp,
		      gfc_get_dtype_rank_type (expr1->rank,type));
    }
  else if (expr1->ts.type == BT_CLASS)
    {
      tree type;
      tmp = gfc_conv_descriptor_dtype (desc);

      if (expr2->ts.type != BT_CLASS)
	type = gfc_typenode_for_spec (&expr2->ts);
      else
	type = gfc_get_character_type_len (1, elemsize2);

      gfc_add_modify (&fblock, tmp,
		      gfc_get_dtype_rank_type (expr2->rank,type));
      /* Set the _len field as well...  */
      if (UNLIMITED_POLY (expr1))
	{
	  tmp = gfc_class_len_get (TREE_OPERAND (desc, 0));
	  if (expr2->ts.type == BT_CHARACTER)
	    gfc_add_modify (&fblock, tmp,
			    fold_convert (TREE_TYPE (tmp),
					  TYPE_SIZE_UNIT (type)));
	  else if (UNLIMITED_POLY (expr2))
	    gfc_add_modify (&fblock, tmp,
			    gfc_class_len_get (TREE_OPERAND (desc2, 0)));
	  else
	    gfc_add_modify (&fblock, tmp,
			    build_int_cst (TREE_TYPE (tmp), 0));
	}
      /* ...and the vptr.  */
      tmp = gfc_class_vptr_get (TREE_OPERAND (desc, 0));
      if (expr2->ts.type == BT_CLASS && !VAR_P (desc2)
	  && TREE_CODE (desc2) == COMPONENT_REF)
	{
	  tmp2 = gfc_get_class_from_expr (desc2);
	  tmp2 = gfc_class_vptr_get (tmp2);
	}
      else if (expr2->ts.type == BT_CLASS && class_expr2 != NULL_TREE)
	tmp2 = gfc_class_vptr_get (class_expr2);
      else
	{
	  tmp2 = gfc_get_symbol_decl (gfc_find_vtab (&expr2->ts));
	  tmp2 = gfc_build_addr_expr (TREE_TYPE (tmp), tmp2);
	}

      gfc_add_modify (&fblock, tmp, fold_convert (TREE_TYPE (tmp), tmp2));
    }
  else if (coarray && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))
    {
      gfc_add_modify (&fblock, gfc_conv_descriptor_dtype (desc),
		      gfc_get_dtype (TREE_TYPE (desc)));
    }

  /* Realloc expression.  Note that the scalarizer uses desc.data
     in the array reference - (*desc.data)[<element>].  */
  gfc_init_block (&realloc_block);
  gfc_init_se (&caf_se, NULL);

  if (coarray)
    {
      token = gfc_get_ultimate_alloc_ptr_comps_caf_token (&caf_se, expr1);
      if (token == NULL_TREE)
	{
	  tmp = gfc_get_tree_for_caf_expr (expr1);
	  if (POINTER_TYPE_P (TREE_TYPE (tmp)))
	    tmp = build_fold_indirect_ref (tmp);
	  gfc_get_caf_token_offset (&caf_se, &token, NULL, tmp, NULL_TREE,
				    expr1);
	  token = gfc_build_addr_expr (NULL_TREE, token);
	}

      gfc_add_block_to_block (&realloc_block, &caf_se.pre);
    }
  if ((expr1->ts.type == BT_DERIVED)
	&& expr1->ts.u.derived->attr.alloc_comp)
    {
      tmp = gfc_deallocate_alloc_comp_no_caf (expr1->ts.u.derived, old_desc,
					      expr1->rank, true);
      gfc_add_expr_to_block (&realloc_block, tmp);
    }

  if (!coarray)
    {
      tmp = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_REALLOC), 2,
				 fold_convert (pvoid_type_node, array1),
				 size2);
      if (flag_openmp_allocators)
	{
	  tree cond, omp_tmp;
	  cond = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				  gfc_conv_descriptor_version (desc),
				  build_int_cst (integer_type_node, 1));
	  omp_tmp = builtin_decl_explicit (BUILT_IN_GOMP_REALLOC);
	  omp_tmp = build_call_expr_loc (input_location, omp_tmp, 4,
				 fold_convert (pvoid_type_node, array1), size2,
				 build_zero_cst (ptr_type_node),
				 build_zero_cst (ptr_type_node));
	  tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp), cond,
			    omp_tmp, tmp);
	}

      gfc_conv_descriptor_data_set (&realloc_block, desc, tmp);
    }
  else
    {
      tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_caf_deregister, 5, token,
				 build_int_cst (integer_type_node,
					       GFC_CAF_COARRAY_DEALLOCATE_ONLY),
				 null_pointer_node, null_pointer_node,
				 integer_zero_node);
      gfc_add_expr_to_block (&realloc_block, tmp);
      tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_caf_register,
				 7, size2,
				 build_int_cst (integer_type_node,
					   GFC_CAF_COARRAY_ALLOC_ALLOCATE_ONLY),
				 token, gfc_build_addr_expr (NULL_TREE, desc),
				 null_pointer_node, null_pointer_node,
				 integer_zero_node);
      gfc_add_expr_to_block (&realloc_block, tmp);
    }

  if ((expr1->ts.type == BT_DERIVED)
	&& expr1->ts.u.derived->attr.alloc_comp)
    {
      tmp = gfc_nullify_alloc_comp (expr1->ts.u.derived, desc,
				    expr1->rank);
      gfc_add_expr_to_block (&realloc_block, tmp);
    }

  gfc_add_block_to_block (&realloc_block, &caf_se.post);
  realloc_expr = gfc_finish_block (&realloc_block);

  /* Malloc expression.  */
  gfc_init_block (&alloc_block);
  if (!coarray)
    {
      tmp = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_MALLOC),
				 1, size2);
      gfc_conv_descriptor_data_set (&alloc_block,
				    desc, tmp);
    }
  else
    {
      tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_caf_register,
				 7, size2,
				 build_int_cst (integer_type_node,
						GFC_CAF_COARRAY_ALLOC),
				 token, gfc_build_addr_expr (NULL_TREE, desc),
				 null_pointer_node, null_pointer_node,
				 integer_zero_node);
      gfc_add_expr_to_block (&alloc_block, tmp);
    }


  /* We already set the dtype in the case of deferred character
     length arrays and class lvalues.  */
  if (!(GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc))
	&& ((expr1->ts.type == BT_CHARACTER && expr1->ts.deferred)
	    || coarray))
      && expr1->ts.type != BT_CLASS)
    {
      tmp = gfc_conv_descriptor_dtype (desc);
      gfc_add_modify (&alloc_block, tmp, gfc_get_dtype (TREE_TYPE (desc)));
    }

  if ((expr1->ts.type == BT_DERIVED)
	&& expr1->ts.u.derived->attr.alloc_comp)
    {
      tmp = gfc_nullify_alloc_comp (expr1->ts.u.derived, desc,
				    expr1->rank);
      gfc_add_expr_to_block (&alloc_block, tmp);
    }
  alloc_expr = gfc_finish_block (&alloc_block);

  /* Malloc if not allocated; realloc otherwise.  */
  tmp = build3_v (COND_EXPR, cond_null, alloc_expr, realloc_expr);
  gfc_add_expr_to_block (&fblock, tmp);

  /* Make sure that the scalarizer data pointer is updated.  */
  if (linfo->data && VAR_P (linfo->data))
    {
      tmp = gfc_conv_descriptor_data_get (desc);
      gfc_add_modify (&fblock, linfo->data, tmp);
    }

  /* Add the label for same shape lhs and rhs.  */
  tmp = build1_v (LABEL_EXPR, jump_label2);
  gfc_add_expr_to_block (&fblock, tmp);

  return gfc_finish_block (&fblock);
}


/* Initialize class descriptor's TKR information.  */

void
gfc_trans_class_array (gfc_symbol * sym, gfc_wrapped_block * block)
{
  tree type, etype;
  tree tmp;
  tree descriptor;
  stmtblock_t init;
  locus loc;
  int rank;

  /* Make sure the frontend gets these right.  */
  gcc_assert (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	      && (CLASS_DATA (sym)->attr.class_pointer
		  || CLASS_DATA (sym)->attr.allocatable));

  gcc_assert (VAR_P (sym->backend_decl)
	      || TREE_CODE (sym->backend_decl) == PARM_DECL);

  if (sym->attr.dummy)
    return;

  descriptor = gfc_class_data_get (sym->backend_decl);
  type = TREE_TYPE (descriptor);

  if (type == NULL || !GFC_DESCRIPTOR_TYPE_P (type))
    return;

  gfc_save_backend_locus (&loc);
  gfc_set_backend_locus (&sym->declared_at);
  gfc_init_block (&init);

  rank = CLASS_DATA (sym)->as ? (CLASS_DATA (sym)->as->rank) : (0);
  gcc_assert (rank>=0);
  tmp = gfc_conv_descriptor_dtype (descriptor);
  etype = gfc_get_element_type (type);
  tmp = fold_build2_loc (input_location, MODIFY_EXPR, TREE_TYPE (tmp), tmp,
			 gfc_get_dtype_rank_type (rank, etype));
  gfc_add_expr_to_block (&init, tmp);

  gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
  gfc_restore_backend_locus (&loc);
}


/* NULLIFY an allocatable/pointer array on function entry, free it on exit.
   Do likewise, recursively if necessary, with the allocatable components of
   derived types.  This function is also called for assumed-rank arrays, which
   are always dummy arguments.  */

void
gfc_trans_deferred_array (gfc_symbol * sym, gfc_wrapped_block * block)
{
  tree type;
  tree tmp;
  tree descriptor;
  stmtblock_t init;
  stmtblock_t cleanup;
  locus loc;
  int rank;
  bool sym_has_alloc_comp, has_finalizer;

  sym_has_alloc_comp = (sym->ts.type == BT_DERIVED
			|| sym->ts.type == BT_CLASS)
			  && sym->ts.u.derived->attr.alloc_comp;
  has_finalizer = gfc_may_be_finalized (sym->ts);

  /* Make sure the frontend gets these right.  */
  gcc_assert (sym->attr.pointer || sym->attr.allocatable || sym_has_alloc_comp
	      || has_finalizer
	      || (sym->as->type == AS_ASSUMED_RANK && sym->attr.dummy));

  gfc_save_backend_locus (&loc);
  gfc_set_backend_locus (&sym->declared_at);
  gfc_init_block (&init);

  gcc_assert (VAR_P (sym->backend_decl)
	      || TREE_CODE (sym->backend_decl) == PARM_DECL);

  if (sym->ts.type == BT_CHARACTER
      && !INTEGER_CST_P (sym->ts.u.cl->backend_decl))
    {
      if (sym->ts.deferred && !sym->ts.u.cl->length && !sym->attr.dummy)
	gfc_add_modify (&init, sym->ts.u.cl->backend_decl,
			build_zero_cst (TREE_TYPE (sym->ts.u.cl->backend_decl)));
      gfc_conv_string_length (sym->ts.u.cl, NULL, &init);
      gfc_trans_vla_type_sizes (sym, &init);

      /* Presence check of optional deferred-length character dummy.  */
      if (sym->ts.deferred && sym->attr.dummy && sym->attr.optional)
	{
	  tmp = gfc_finish_block (&init);
	  tmp = build3_v (COND_EXPR, gfc_conv_expr_present (sym),
			  tmp, build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&init, tmp);
	}
    }

  /* Dummy, use associated and result variables don't need anything special.  */
  if (sym->attr.dummy || sym->attr.use_assoc || sym->attr.result)
    {
      gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
      gfc_restore_backend_locus (&loc);
      return;
    }

  descriptor = sym->backend_decl;

  /* Although static, derived types with default initializers and
     allocatable components must not be nulled wholesale; instead they
     are treated component by component.  */
  if (TREE_STATIC (descriptor) && !sym_has_alloc_comp && !has_finalizer)
    {
      /* SAVEd variables are not freed on exit.  */
      gfc_trans_static_array_pointer (sym);

      gfc_add_init_cleanup (block, gfc_finish_block (&init), NULL_TREE);
      gfc_restore_backend_locus (&loc);
      return;
    }

  /* Get the descriptor type.  */
  type = TREE_TYPE (sym->backend_decl);

  if ((sym_has_alloc_comp || (has_finalizer && sym->ts.type != BT_CLASS))
      && !(sym->attr.pointer || sym->attr.allocatable))
    {
      if (!sym->attr.save
	  && !(TREE_STATIC (sym->backend_decl) && sym->attr.is_main_program))
	{
	  if (sym->value == NULL
	      || !gfc_has_default_initializer (sym->ts.u.derived))
	    {
	      rank = sym->as ? sym->as->rank : 0;
	      tmp = gfc_nullify_alloc_comp (sym->ts.u.derived,
					    descriptor, rank);
	      gfc_add_expr_to_block (&init, tmp);
	    }
	  else
	    gfc_init_default_dt (sym, &init, false);
	}
    }
  else if (!GFC_DESCRIPTOR_TYPE_P (type))
    {
      /* If the backend_decl is not a descriptor, we must have a pointer
	 to one.  */
      descriptor = build_fold_indirect_ref_loc (input_location,
						sym->backend_decl);
      type = TREE_TYPE (descriptor);
    }

  /* NULLIFY the data pointer, for non-saved allocatables.  */
  if (GFC_DESCRIPTOR_TYPE_P (type) && !sym->attr.save && sym->attr.allocatable)
    {
      gfc_conv_descriptor_data_set (&init, descriptor, null_pointer_node);
      if (flag_coarray == GFC_FCOARRAY_LIB && sym->attr.codimension)
	{
	  /* Declare the variable static so its array descriptor stays present
	     after leaving the scope.  It may still be accessed through another
	     image.  This may happen, for example, with the caf_mpi
	     implementation.  */
	  TREE_STATIC (descriptor) = 1;
	  tmp = gfc_conv_descriptor_token (descriptor);
	  gfc_add_modify (&init, tmp, fold_convert (TREE_TYPE (tmp),
						    null_pointer_node));
	}
    }

  /* Set initial TKR for pointers and allocatables */
  if (GFC_DESCRIPTOR_TYPE_P (type)
      && (sym->attr.pointer || sym->attr.allocatable))
    {
      tree etype;

      gcc_assert (sym->as && sym->as->rank>=0);
      tmp = gfc_conv_descriptor_dtype (descriptor);
      etype = gfc_get_element_type (type);
      tmp = fold_build2_loc (input_location, MODIFY_EXPR,
  			     TREE_TYPE (tmp), tmp,
  			     gfc_get_dtype_rank_type (sym->as->rank, etype));
      gfc_add_expr_to_block (&init, tmp);
    }
  gfc_restore_backend_locus (&loc);
  gfc_init_block (&cleanup);

  /* Allocatable arrays need to be freed when they go out of scope.
     The allocatable components of pointers must not be touched.  */
  if (!sym->attr.allocatable && has_finalizer && sym->ts.type != BT_CLASS
      && !sym->attr.pointer && !sym->attr.artificial && !sym->attr.save
      && !sym->ns->proc_name->attr.is_main_program)
    {
      gfc_expr *e;
      sym->attr.referenced = 1;
      e = gfc_lval_expr_from_sym (sym);
      gfc_add_finalizer_call (&cleanup, e);
      gfc_free_expr (e);
    }
  else if ((!sym->attr.allocatable || !has_finalizer)
      && sym_has_alloc_comp && !(sym->attr.function || sym->attr.result)
      && !sym->attr.pointer && !sym->attr.save
      && !(sym->attr.artificial && sym->name[0] == '_')
      && !sym->ns->proc_name->attr.is_main_program)
    {
      int rank;
      rank = sym->as ? sym->as->rank : 0;
      tmp = gfc_deallocate_alloc_comp (sym->ts.u.derived, descriptor, rank,
				       (sym->attr.codimension
					&& flag_coarray == GFC_FCOARRAY_LIB)
				       ? GFC_STRUCTURE_CAF_MODE_IN_COARRAY
				       : 0);
      gfc_add_expr_to_block (&cleanup, tmp);
    }

  if (sym->attr.allocatable && (sym->attr.dimension || sym->attr.codimension)
      && !sym->attr.save && !sym->attr.result
      && !sym->ns->proc_name->attr.is_main_program)
    {
      gfc_expr *e;
      e = has_finalizer ? gfc_lval_expr_from_sym (sym) : NULL;
      tmp = gfc_deallocate_with_status (sym->backend_decl, NULL_TREE, NULL_TREE,
					NULL_TREE, NULL_TREE, true, e,
					sym->attr.codimension
					? GFC_CAF_COARRAY_DEREGISTER
					: GFC_CAF_COARRAY_NOCOARRAY,
					NULL_TREE, gfc_finish_block (&cleanup));
      if (e)
	gfc_free_expr (e);
      gfc_init_block (&cleanup);
      gfc_add_expr_to_block (&cleanup, tmp);
    }

  gfc_add_init_cleanup (block, gfc_finish_block (&init),
			gfc_finish_block (&cleanup));
}

/************ Expression Walking Functions ******************/

/* Walk a variable reference.

   Possible extension - multiple component subscripts.
    x(:,:) = foo%a(:)%b(:)
   Transforms to
    forall (i=..., j=...)
      x(i,j) = foo%a(j)%b(i)
    end forall
   This adds a fair amount of complexity because you need to deal with more
   than one ref.  Maybe handle in a similar manner to vector subscripts.
   Maybe not worth the effort.  */


static gfc_ss *
gfc_walk_variable_expr (gfc_ss * ss, gfc_expr * expr)
{
  gfc_ref *ref;

  gfc_fix_class_refs (expr);

  for (ref = expr->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.type != AR_ELEMENT)
      break;

  return gfc_walk_array_ref (ss, expr, ref);
}


gfc_ss *
gfc_walk_array_ref (gfc_ss * ss, gfc_expr * expr, gfc_ref * ref)
{
  gfc_array_ref *ar;
  gfc_ss *newss;
  int n;

  for (; ref; ref = ref->next)
    {
      if (ref->type == REF_SUBSTRING)
	{
	  ss = gfc_get_scalar_ss (ss, ref->u.ss.start);
	  if (ref->u.ss.end)
	    ss = gfc_get_scalar_ss (ss, ref->u.ss.end);
	}

      /* We're only interested in array sections from now on.  */
      if (ref->type != REF_ARRAY)
	continue;

      ar = &ref->u.ar;

      switch (ar->type)
	{
	case AR_ELEMENT:
	  for (n = ar->dimen - 1; n >= 0; n--)
	    ss = gfc_get_scalar_ss (ss, ar->start[n]);
	  break;

	case AR_FULL:
	  /* Assumed shape arrays from interface mapping need this fix.  */
	  if (!ar->as && expr->symtree->n.sym->as)
	    {
	      ar->as = gfc_get_array_spec();
	      *ar->as = *expr->symtree->n.sym->as;
	    }
	  newss = gfc_get_array_ss (ss, expr, ar->as->rank, GFC_SS_SECTION);
	  newss->info->data.array.ref = ref;

	  /* Make sure array is the same as array(:,:), this way
	     we don't need to special case all the time.  */
	  ar->dimen = ar->as->rank;
	  for (n = 0; n < ar->dimen; n++)
	    {
	      ar->dimen_type[n] = DIMEN_RANGE;

	      gcc_assert (ar->start[n] == NULL);
	      gcc_assert (ar->end[n] == NULL);
	      gcc_assert (ar->stride[n] == NULL);
	    }
	  ss = newss;
	  break;

	case AR_SECTION:
	  newss = gfc_get_array_ss (ss, expr, 0, GFC_SS_SECTION);
	  newss->info->data.array.ref = ref;

	  /* We add SS chains for all the subscripts in the section.  */
	  for (n = 0; n < ar->dimen; n++)
	    {
	      gfc_ss *indexss;

	      switch (ar->dimen_type[n])
		{
		case DIMEN_ELEMENT:
		  /* Add SS for elemental (scalar) subscripts.  */
		  gcc_assert (ar->start[n]);
		  indexss = gfc_get_scalar_ss (gfc_ss_terminator, ar->start[n]);
		  indexss->loop_chain = gfc_ss_terminator;
		  newss->info->data.array.subscript[n] = indexss;
		  break;

		case DIMEN_RANGE:
                  /* We don't add anything for sections, just remember this
                     dimension for later.  */
		  newss->dim[newss->dimen] = n;
		  newss->dimen++;
		  break;

		case DIMEN_VECTOR:
		  /* Create a GFC_SS_VECTOR index in which we can store
		     the vector's descriptor.  */
		  indexss = gfc_get_array_ss (gfc_ss_terminator, ar->start[n],
					      1, GFC_SS_VECTOR);
		  indexss->loop_chain = gfc_ss_terminator;
		  newss->info->data.array.subscript[n] = indexss;
		  newss->dim[newss->dimen] = n;
		  newss->dimen++;
		  break;

		default:
		  /* We should know what sort of section it is by now.  */
		  gcc_unreachable ();
		}
	    }
	  /* We should have at least one non-elemental dimension,
	     unless we are creating a descriptor for a (scalar) coarray.  */
	  gcc_assert (newss->dimen > 0
		      || newss->info->data.array.ref->u.ar.as->corank > 0);
	  ss = newss;
	  break;

	default:
	  /* We should know what sort of section it is by now.  */
	  gcc_unreachable ();
	}

    }
  return ss;
}


/* Walk an expression operator. If only one operand of a binary expression is
   scalar, we must also add the scalar term to the SS chain.  */

static gfc_ss *
gfc_walk_op_expr (gfc_ss * ss, gfc_expr * expr)
{
  gfc_ss *head;
  gfc_ss *head2;

  head = gfc_walk_subexpr (ss, expr->value.op.op1);
  if (expr->value.op.op2 == NULL)
    head2 = head;
  else
    head2 = gfc_walk_subexpr (head, expr->value.op.op2);

  /* All operands are scalar.  Pass back and let the caller deal with it.  */
  if (head2 == ss)
    return head2;

  /* All operands require scalarization.  */
  if (head != ss && (expr->value.op.op2 == NULL || head2 != head))
    return head2;

  /* One of the operands needs scalarization, the other is scalar.
     Create a gfc_ss for the scalar expression.  */
  if (head == ss)
    {
      /* First operand is scalar.  We build the chain in reverse order, so
         add the scalar SS after the second operand.  */
      head = head2;
      while (head && head->next != ss)
	head = head->next;
      /* Check we haven't somehow broken the chain.  */
      gcc_assert (head);
      head->next = gfc_get_scalar_ss (ss, expr->value.op.op1);
    }
  else				/* head2 == head */
    {
      gcc_assert (head2 == head);
      /* Second operand is scalar.  */
      head2 = gfc_get_scalar_ss (head2, expr->value.op.op2);
    }

  return head2;
}


/* Reverse a SS chain.  */

gfc_ss *
gfc_reverse_ss (gfc_ss * ss)
{
  gfc_ss *next;
  gfc_ss *head;

  gcc_assert (ss != NULL);

  head = gfc_ss_terminator;
  while (ss != gfc_ss_terminator)
    {
      next = ss->next;
      /* Check we didn't somehow break the chain.  */
      gcc_assert (next != NULL);
      ss->next = head;
      head = ss;
      ss = next;
    }

  return (head);
}


/* Given an expression referring to a procedure, return the symbol of its
   interface.  We can't get the procedure symbol directly as we have to handle
   the case of (deferred) type-bound procedures.  */

gfc_symbol *
gfc_get_proc_ifc_for_expr (gfc_expr *procedure_ref)
{
  gfc_symbol *sym;
  gfc_ref *ref;

  if (procedure_ref == NULL)
    return NULL;

  /* Normal procedure case.  */
  if (procedure_ref->expr_type == EXPR_FUNCTION
      && procedure_ref->value.function.esym)
    sym = procedure_ref->value.function.esym;
  else
    sym = procedure_ref->symtree->n.sym;

  /* Typebound procedure case.  */
  for (ref = procedure_ref->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT
	  && ref->u.c.component->attr.proc_pointer)
	sym = ref->u.c.component->ts.interface;
      else
	sym = NULL;
    }

  return sym;
}


/* Given an expression referring to an intrinsic function call,
   return the intrinsic symbol.  */

gfc_intrinsic_sym *
gfc_get_intrinsic_for_expr (gfc_expr *call)
{
  if (call == NULL)
    return NULL;

  /* Normal procedure case.  */
  if (call->expr_type == EXPR_FUNCTION)
    return call->value.function.isym;
  else
    return NULL;
}


/* Indicates whether an argument to an intrinsic function should be used in
   scalarization.  It is usually the case, except for some intrinsics
   requiring the value to be constant, and using the value at compile time only.
   As the value is not used at runtime in those cases, we don’t produce code
   for it, and it should not be visible to the scalarizer.
   FUNCTION is the intrinsic function being called, ACTUAL_ARG is the actual
   argument being examined in that call, and ARG_NUM the index number
   of ACTUAL_ARG in the list of arguments.
   The intrinsic procedure’s dummy argument associated with ACTUAL_ARG is
   identified using the name in ACTUAL_ARG if it is present (that is: if it’s
   a keyword argument), otherwise using ARG_NUM.  */

static bool
arg_evaluated_for_scalarization (gfc_intrinsic_sym *function,
				 gfc_dummy_arg *dummy_arg)
{
  if (function != NULL && dummy_arg != NULL)
    {
      switch (function->id)
	{
	  case GFC_ISYM_INDEX:
	  case GFC_ISYM_LEN_TRIM:
	  case GFC_ISYM_MASKL:
	  case GFC_ISYM_MASKR:
	  case GFC_ISYM_SCAN:
	  case GFC_ISYM_VERIFY:
	    if (strcmp ("kind", gfc_dummy_arg_get_name (*dummy_arg)) == 0)
	      return false;
	  /* Fallthrough.  */

	  default:
	    break;
	}
    }

  return true;
}


/* Walk the arguments of an elemental function.
   PROC_EXPR is used to check whether an argument is permitted to be absent.  If
   it is NULL, we don't do the check and the argument is assumed to be present.
*/

gfc_ss *
gfc_walk_elemental_function_args (gfc_ss * ss, gfc_actual_arglist *arg,
				  gfc_intrinsic_sym *intrinsic_sym,
				  gfc_ss_type type)
{
  int scalar;
  gfc_ss *head;
  gfc_ss *tail;
  gfc_ss *newss;

  head = gfc_ss_terminator;
  tail = NULL;

  scalar = 1;
  for (; arg; arg = arg->next)
    {
      gfc_dummy_arg * const dummy_arg = arg->associated_dummy;
      if (!arg->expr
	  || arg->expr->expr_type == EXPR_NULL
	  || !arg_evaluated_for_scalarization (intrinsic_sym, dummy_arg))
	continue;

      newss = gfc_walk_subexpr (head, arg->expr);
      if (newss == head)
	{
	  /* Scalar argument.  */
	  gcc_assert (type == GFC_SS_SCALAR || type == GFC_SS_REFERENCE);
	  newss = gfc_get_scalar_ss (head, arg->expr);
	  newss->info->type = type;
	  if (dummy_arg)
	    newss->info->data.scalar.dummy_arg = dummy_arg;
	}
      else
	scalar = 0;

      if (dummy_arg != NULL
	  && gfc_dummy_arg_is_optional (*dummy_arg)
	  && arg->expr->expr_type == EXPR_VARIABLE
	  && (gfc_expr_attr (arg->expr).optional
	      || gfc_expr_attr (arg->expr).allocatable
	      || gfc_expr_attr (arg->expr).pointer))
	newss->info->can_be_null_ref = true;

      head = newss;
      if (!tail)
        {
          tail = head;
          while (tail->next != gfc_ss_terminator)
            tail = tail->next;
        }
    }

  if (scalar)
    {
      /* If all the arguments are scalar we don't need the argument SS.  */
      gfc_free_ss_chain (head);
      /* Pass it back.  */
      return ss;
    }

  /* Add it onto the existing chain.  */
  tail->next = ss;
  return head;
}


/* Walk a function call.  Scalar functions are passed back, and taken out of
   scalarization loops.  For elemental functions we walk their arguments.
   The result of functions returning arrays is stored in a temporary outside
   the loop, so that the function is only called once.  Hence we do not need
   to walk their arguments.  */

static gfc_ss *
gfc_walk_function_expr (gfc_ss * ss, gfc_expr * expr)
{
  gfc_intrinsic_sym *isym;
  gfc_symbol *sym;
  gfc_component *comp = NULL;

  isym = expr->value.function.isym;

  /* Handle intrinsic functions separately.  */
  if (isym)
    return gfc_walk_intrinsic_function (ss, expr, isym);

  sym = expr->value.function.esym;
  if (!sym)
    sym = expr->symtree->n.sym;

  if (gfc_is_class_array_function (expr))
    return gfc_get_array_ss (ss, expr,
			     CLASS_DATA (expr->value.function.esym->result)->as->rank,
			     GFC_SS_FUNCTION);

  /* A function that returns arrays.  */
  comp = gfc_get_proc_ptr_comp (expr);
  if ((!comp && gfc_return_by_reference (sym) && sym->result->attr.dimension)
      || (comp && comp->attr.dimension))
    return gfc_get_array_ss (ss, expr, expr->rank, GFC_SS_FUNCTION);

  /* Walk the parameters of an elemental function.  For now we always pass
     by reference.  */
  if (sym->attr.elemental || (comp && comp->attr.elemental))
    {
      gfc_ss *old_ss = ss;

      ss = gfc_walk_elemental_function_args (old_ss,
					     expr->value.function.actual,
					     gfc_get_intrinsic_for_expr (expr),
					     GFC_SS_REFERENCE);
      if (ss != old_ss
	  && (comp
	      || sym->attr.proc_pointer
	      || sym->attr.if_source != IFSRC_DECL
	      || sym->attr.array_outer_dependency))
	ss->info->array_outer_dependency = 1;
    }

  /* Scalar functions are OK as these are evaluated outside the scalarization
     loop.  Pass back and let the caller deal with it.  */
  return ss;
}


/* An array temporary is constructed for array constructors.  */

static gfc_ss *
gfc_walk_array_constructor (gfc_ss * ss, gfc_expr * expr)
{
  return gfc_get_array_ss (ss, expr, expr->rank, GFC_SS_CONSTRUCTOR);
}


/* Walk an expression.  Add walked expressions to the head of the SS chain.
   A wholly scalar expression will not be added.  */

gfc_ss *
gfc_walk_subexpr (gfc_ss * ss, gfc_expr * expr)
{
  gfc_ss *head;

  switch (expr->expr_type)
    {
    case EXPR_VARIABLE:
      head = gfc_walk_variable_expr (ss, expr);
      return head;

    case EXPR_OP:
      head = gfc_walk_op_expr (ss, expr);
      return head;

    case EXPR_FUNCTION:
      head = gfc_walk_function_expr (ss, expr);
      return head;

    case EXPR_CONSTANT:
    case EXPR_NULL:
    case EXPR_STRUCTURE:
      /* Pass back and let the caller deal with it.  */
      break;

    case EXPR_ARRAY:
      head = gfc_walk_array_constructor (ss, expr);
      return head;

    case EXPR_SUBSTRING:
      /* Pass back and let the caller deal with it.  */
      break;

    default:
      gfc_internal_error ("bad expression type during walk (%d)",
		      expr->expr_type);
    }
  return ss;
}


/* Entry point for expression walking.
   A return value equal to the passed chain means this is
   a scalar expression.  It is up to the caller to take whatever action is
   necessary to translate these.  */

gfc_ss *
gfc_walk_expr (gfc_expr * expr)
{
  gfc_ss *res;

  res = gfc_walk_subexpr (gfc_ss_terminator, expr);
  return gfc_reverse_ss (res);
}
