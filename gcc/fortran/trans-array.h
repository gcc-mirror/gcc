/* Header for array handling functions
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Paul Brook

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* Generate code to free an array.  */
tree gfc_array_deallocate (tree, tree);

/* Generate code to initialize an allocate an array.  Statements are added to
   se, which should contain an expression for the array descriptor.  */
void gfc_array_allocate (gfc_se *, gfc_ref *, tree);

/* Allow the bounds of a loop to be set from a callee's array spec.  */
void gfc_set_loop_bounds_from_array_spec (gfc_interface_mapping *,
					  gfc_se *, gfc_array_spec *);

/* Generate code to allocate a temporary array.  */
tree gfc_trans_allocate_temp_array (stmtblock_t *, stmtblock_t *,
				    gfc_loopinfo *, gfc_ss_info *, tree, bool);

/* Generate function entry code for allocation of compiler allocated array
   variables.  */
tree gfc_trans_auto_array_allocation (tree, gfc_symbol *, tree);
/* Generate entry and exit code for dummy array parameters.  */
tree gfc_trans_dummy_array_bias (gfc_symbol *, tree, tree);
/* Generate entry and exit code for g77 calling convention arrays.  */
tree gfc_trans_g77_array (gfc_symbol *, tree);
/* Add initialization for deferred arrays.  */
tree gfc_trans_deferred_array (gfc_symbol *, tree);
/* Generate an initializer for a static pointer or allocatable array.  */
void gfc_trans_static_array_pointer (gfc_symbol *);

/* Generate scalarization information for an expression.  */
gfc_ss *gfc_walk_expr (gfc_expr *);
/* Walk the arguments of an elemental function.  */
gfc_ss *gfc_walk_elemental_function_args (gfc_ss *, gfc_actual_arglist *,
					  gfc_ss_type);
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

/* Calculates the lower bound and stride of array sections.  */
void gfc_conv_ss_startstride (gfc_loopinfo *);

void gfc_init_loopinfo (gfc_loopinfo *);
void gfc_copy_loopinfo_to_se (gfc_se *, gfc_loopinfo *);

/* Marks the start of a scalarized expression, and declares loop variables.  */
void gfc_start_scalarized_body (gfc_loopinfo *, stmtblock_t *);
/* Generates the actual loops for a scalarized expression.  */
void gfc_trans_scalarizing_loops (gfc_loopinfo *, stmtblock_t *);
/* Mark the end of the main loop body and the start of the copying loop.  */
void gfc_trans_scalarized_loop_boundary (gfc_loopinfo *, stmtblock_t *);
/* Initialize the scalarization loop parameters.  */
void gfc_conv_loop_setup (gfc_loopinfo *);
/* Resolve array assignment dependencies.  */
void gfc_conv_resolve_dependencies (gfc_loopinfo *, gfc_ss *, gfc_ss *);
/* Build a null array descriptor constructor.  */
tree gfc_build_null_descriptor (tree);

/* Get a single array element.  */
void gfc_conv_array_ref (gfc_se *, gfc_array_ref *);
/* Translate a reference to a temporary array.  */
void gfc_conv_tmp_array_ref (gfc_se * se);
/* Translate a reference to an array temporary.  */
void gfc_conv_tmp_ref (gfc_se *);

/* Evaluate an array expression.  */
void gfc_conv_expr_descriptor (gfc_se *, gfc_expr *, gfc_ss *);
/* Convert an array for passing as an actual function parameter.  */
void gfc_conv_array_parameter (gfc_se *, gfc_expr *, gfc_ss *, int);

/* These work with both descriptors and descriptorless arrays.  */
tree gfc_conv_array_data (tree);
tree gfc_conv_array_offset (tree);
/* Return either an INT_CST or an expression for that part of the descriptor.  */
tree gfc_conv_array_stride (tree, int);
tree gfc_conv_array_lbound (tree, int);
tree gfc_conv_array_ubound (tree, int);

/* Build expressions for accessing components of an array descriptor.  */
tree gfc_conv_descriptor_data_get (tree);
void gfc_conv_descriptor_data_set (stmtblock_t *, tree, tree);
tree gfc_conv_descriptor_data_addr (tree);
tree gfc_conv_descriptor_offset (tree);
tree gfc_conv_descriptor_dtype (tree);
tree gfc_conv_descriptor_stride (tree, tree);
tree gfc_conv_descriptor_lbound (tree, tree);
tree gfc_conv_descriptor_ubound (tree, tree);

/* Dependency checking for WHERE and FORALL.  */
int gfc_check_dependency (gfc_expr *, gfc_expr *, gfc_expr **, int);
/* Dependency checking for function calls.  */
int gfc_check_fncall_dependency (gfc_expr *, gfc_expr *);

/* Add pre-loop scalarization code for intrinsic functions which require
   special handling.  */
void gfc_add_intrinsic_ss_code (gfc_loopinfo *, gfc_ss *);
