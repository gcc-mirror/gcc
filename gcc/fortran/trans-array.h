/* Header for array handling functions
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2012
   Free Software Foundation, Inc.
   Contributed by Paul Brook

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

/* Generate code to free an array.  */
tree gfc_array_deallocate (tree, tree, tree, tree, tree, gfc_expr*);

/* Generate code to initialize and allocate an array.  Statements are added to
   se, which should contain an expression for the array descriptor.  */
bool gfc_array_allocate (gfc_se *, gfc_expr *, tree, tree, tree, tree,
			 tree, tree *, gfc_expr *);

/* Allow the bounds of a loop to be set from a callee's array spec.  */
void gfc_set_loop_bounds_from_array_spec (gfc_interface_mapping *,
					  gfc_se *, gfc_array_spec *);

/* Generate code to create a temporary array.  */
tree gfc_trans_create_temp_array (stmtblock_t *, stmtblock_t *, gfc_ss *,
				  tree, tree, bool, bool, bool, locus *);

/* Generate function entry code for allocation of compiler allocated array
   variables.  */
void gfc_trans_auto_array_allocation (tree, gfc_symbol *, gfc_wrapped_block *);
/* Generate entry and exit code for dummy array parameters.  */
void gfc_trans_dummy_array_bias (gfc_symbol *, tree, gfc_wrapped_block *);
/* Generate entry and exit code for g77 calling convention arrays.  */
void gfc_trans_g77_array (gfc_symbol *, gfc_wrapped_block *);
/* Generate code to deallocate an array, if it is allocated.  */
tree gfc_trans_dealloc_allocated (tree, bool);

tree gfc_duplicate_allocatable (tree dest, tree src, tree type, int rank);

tree gfc_copy_allocatable_data (tree dest, tree src, tree type, int rank);

tree gfc_nullify_alloc_comp (gfc_symbol *, tree, int);

tree gfc_deallocate_alloc_comp (gfc_symbol *, tree, int);

tree gfc_copy_alloc_comp (gfc_symbol *, tree, tree, int);

tree gfc_copy_only_alloc_comp (gfc_symbol *, tree, tree, int);

tree gfc_alloc_allocatable_for_assignment (gfc_loopinfo*, gfc_expr*, gfc_expr*);

bool gfc_is_reallocatable_lhs (gfc_expr *);

/* Add initialization for deferred arrays.  */
void gfc_trans_deferred_array (gfc_symbol *, gfc_wrapped_block *);
/* Generate an initializer for a static pointer or allocatable array.  */
void gfc_trans_static_array_pointer (gfc_symbol *);

/* Get the procedure interface for a function call.  */
gfc_symbol *gfc_get_proc_ifc_for_expr (gfc_expr *);
/* Generate scalarization information for an expression.  */
gfc_ss *gfc_walk_expr (gfc_expr *);
/* Workhorse for gfc_walk_expr.  */
gfc_ss *gfc_walk_subexpr (gfc_ss *, gfc_expr *);
/* Workhorse for gfc_walk_variable_expr.  */
gfc_ss *gfc_walk_array_ref (gfc_ss *, gfc_expr *, gfc_ref * ref);
/* Walk the arguments of an elemental function.  */
gfc_ss *gfc_walk_elemental_function_args (gfc_ss *, gfc_actual_arglist *,
					  gfc_symbol *, gfc_ss_type);
/* Walk an intrinsic function.  */
gfc_ss *gfc_walk_intrinsic_function (gfc_ss *, gfc_expr *,
				     gfc_intrinsic_sym *);
/* Reverse the order of an SS chain.  */
gfc_ss *gfc_reverse_ss (gfc_ss *);

/* Free the SS associated with a loop.  */
void gfc_cleanup_loop (gfc_loopinfo *);
/* Associate a SS chain with a loop.  */
void gfc_add_ss_to_loop (gfc_loopinfo *, gfc_ss *);
/* Mark a SS chain as used in this loop.  */
void gfc_mark_ss_chain_used (gfc_ss *, unsigned);
/* Free a gfc_ss chain.  */
void gfc_free_ss_chain (gfc_ss *);
/* Free a single gfc_ss element.  */
void gfc_free_ss (gfc_ss *);
/* Allocate a new array type ss.  */
gfc_ss *gfc_get_array_ss (gfc_ss *, gfc_expr *, int, gfc_ss_type);
/* Allocate a new temporary type ss.  */
gfc_ss *gfc_get_temp_ss (tree, tree, int);
/* Allocate a new scalar type ss.  */
gfc_ss *gfc_get_scalar_ss (gfc_ss *, gfc_expr *);

/* Calculates the lower bound and stride of array sections.  */
void gfc_conv_ss_startstride (gfc_loopinfo *);

void gfc_init_loopinfo (gfc_loopinfo *);
void gfc_copy_loopinfo_to_se (gfc_se *, gfc_loopinfo *);

/* Marks the start of a scalarized expression, and declares loop variables.  */
void gfc_start_scalarized_body (gfc_loopinfo *, stmtblock_t *);
/* Generates one actual loop for a scalarized expression.  */
void  gfc_trans_scalarized_loop_end (gfc_loopinfo *, int, stmtblock_t *);
/* Generates the actual loops for a scalarized expression.  */
void gfc_trans_scalarizing_loops (gfc_loopinfo *, stmtblock_t *);
/* Mark the end of the main loop body and the start of the copying loop.  */
void gfc_trans_scalarized_loop_boundary (gfc_loopinfo *, stmtblock_t *);
/* Initialize the scalarization loop parameters.  */
void gfc_conv_loop_setup (gfc_loopinfo *, locus *);
/* Set each array's delta.  */
void gfc_set_delta (gfc_loopinfo *);
/* Resolve array assignment dependencies.  */
void gfc_conv_resolve_dependencies (gfc_loopinfo *, gfc_ss *, gfc_ss *);
/* Build a null array descriptor constructor.  */
tree gfc_build_null_descriptor (tree);

/* Get a single array element.  */
void gfc_conv_array_ref (gfc_se *, gfc_array_ref *, gfc_symbol *, locus *);
/* Translate a reference to a temporary array.  */
void gfc_conv_tmp_array_ref (gfc_se * se);
/* Translate a reference to an array temporary.  */
void gfc_conv_tmp_ref (gfc_se *);

/* Evaluate an array expression.  */
void gfc_conv_expr_descriptor (gfc_se *, gfc_expr *, gfc_ss *);
/* Convert an array for passing as an actual function parameter.  */
void gfc_conv_array_parameter (gfc_se *, gfc_expr *, gfc_ss *, bool,
			       const gfc_symbol *, const char *, tree *);
/* Evaluate and transpose a matrix expression.  */
void gfc_conv_array_transpose (gfc_se *, gfc_expr *);

/* These work with both descriptors and descriptorless arrays.  */
tree gfc_conv_array_data (tree);
tree gfc_conv_array_offset (tree);
/* Return either an INT_CST or an expression for that part of the descriptor.  */
tree gfc_conv_array_stride (tree, int);
tree gfc_conv_array_lbound (tree, int);
tree gfc_conv_array_ubound (tree, int);

/* Set cobounds of an array.  */
void gfc_trans_array_cobounds (tree, stmtblock_t *, const gfc_symbol *);

/* Build expressions for accessing components of an array descriptor.  */
tree gfc_conv_descriptor_data_get (tree);
tree gfc_conv_descriptor_data_addr (tree);
tree gfc_conv_descriptor_offset_get (tree);
tree gfc_conv_descriptor_dtype (tree);
tree gfc_conv_descriptor_rank (tree);
tree gfc_get_descriptor_dimension (tree);
tree gfc_conv_descriptor_stride_get (tree, tree);
tree gfc_conv_descriptor_lbound_get (tree, tree);
tree gfc_conv_descriptor_ubound_get (tree, tree);
tree gfc_conv_descriptor_token (tree);

void gfc_conv_descriptor_data_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_offset_set (stmtblock_t *, tree, tree);
void gfc_conv_descriptor_stride_set (stmtblock_t *, tree, tree, tree);
void gfc_conv_descriptor_lbound_set (stmtblock_t *, tree, tree, tree);
void gfc_conv_descriptor_ubound_set (stmtblock_t *, tree, tree, tree);

/* Shift lower bound of descriptor, updating ubound and offset.  */
void gfc_conv_shift_descriptor_lbound (stmtblock_t*, tree, int, tree);

/* Add pre-loop scalarization code for intrinsic functions which require
   special handling.  */
void gfc_add_intrinsic_ss_code (gfc_loopinfo *, gfc_ss *);

/* Functions for constant array constructor processing.  */
unsigned HOST_WIDE_INT gfc_constant_array_constructor_p (gfc_constructor_base);
tree gfc_build_constant_array_constructor (gfc_expr *, tree);

/* Copy a string from src to dest.  */
void gfc_trans_string_copy (stmtblock_t *, tree, tree, int, tree, tree, int);

/* Calculate extent / size of an array.  */
tree gfc_conv_array_extent_dim (tree, tree, tree*);
tree gfc_conv_descriptor_size (tree, int);
tree gfc_conv_descriptor_cosize (tree, int, int);
