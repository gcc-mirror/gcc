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

#ifndef GFC_TRANS_DESCRIPTOR_H
#define GFC_TRANS_DESCRIPTOR_H


tree gfc_get_descriptor_dimension (tree);
tree gfc_conv_descriptor_token (tree);

tree gfc_conv_descriptor_data_get (tree);
tree gfc_conv_descriptor_offset_get (tree);
tree gfc_conv_descriptor_dtype_get (tree);
tree gfc_conv_descriptor_elem_len_get (tree);
tree gfc_conv_descriptor_version_get (tree);
tree gfc_conv_descriptor_rank_get (tree);
tree gfc_conv_descriptor_type_get (tree);
tree gfc_conv_descriptor_span_get (tree);

tree gfc_conv_descriptor_stride_get (tree, tree);
tree gfc_conv_descriptor_lbound_get (tree, tree);
tree gfc_conv_descriptor_ubound_get (tree, tree);

void gfc_conv_descriptor_data_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_offset_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_dtype_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_elem_len_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_version_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_rank_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_rank_set (stmtblock_t *, tree, int);
void gfc_conv_descriptor_type_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_type_set (stmtblock_t *, tree, int);
tree gfc_conv_descriptor_type_set (tree, tree);
tree gfc_conv_descriptor_type_set (tree, int);
void gfc_conv_descriptor_span_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_stride_set (stmtblock_t *, tree, tree, tree);
void gfc_conv_descriptor_lbound_set (stmtblock_t *, tree, tree, tree);
void gfc_conv_descriptor_ubound_set (stmtblock_t *, tree, tree, tree);
void gfc_conv_descriptor_token_set (stmtblock_t *block, tree desc, tree value);

tree gfc_build_dtype_constructor (tree, int, int);

/* Build expressions for accessing components of an array descriptor.  */
void gfc_get_descriptor_offsets_for_info (const_tree, tree *, tree *, tree *,
					  tree *, tree *, tree *, tree *,
					  tree *);

/* Build a null array descriptor constructor.  */
tree gfc_build_null_descriptor (tree type);

void gfc_nullify_descriptor (stmtblock_t *block, tree);
void gfc_init_result_descriptor (stmtblock_t *block, tree descr);
void gfc_init_absent_descriptor (stmtblock_t *block, tree descr);
void gfc_init_descriptor_variable (stmtblock_t *block, gfc_symbol *sym,
				   tree descr);
tree gfc_create_unallocated_library_result_descriptor (stmtblock_t *, tree,
						       tree);
tree gfc_create_null_actual_descriptor (stmtblock_t *, gfc_typespec *,
					symbol_attribute, int);

tree gfc_conv_descriptor_size (tree, int);
tree gfc_conv_descriptor_cosize (tree, int, int);

/* Shift lower bound of descriptor, updating ubound and offset.  */
void gfc_conv_shift_descriptor_lbound (stmtblock_t*, tree, int, tree);

void gfc_copy_descriptor (stmtblock_t *, tree, tree, int);

void gfc_grow_array (stmtblock_t *, tree, tree);

#endif /* GFC_TRANS_DESCRIPTOR_H */
