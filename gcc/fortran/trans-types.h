/* Header for Fortran 95 types backend support.
   Copyright (C) 2002-2022 Free Software Foundation, Inc.
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


#ifndef GFC_BACKEND_H
#define GFC_BACKEND_H


extern GTY(()) tree gfc_array_index_type;
extern GTY(()) tree gfc_array_range_type;
extern GTY(()) tree gfc_character1_type_node;
extern GTY(()) tree ppvoid_type_node;
extern GTY(()) tree pvoid_type_node;
extern GTY(()) tree prvoid_type_node;
extern GTY(()) tree pchar_type_node;
extern GTY(()) tree gfc_float128_type_node;
extern GTY(()) tree gfc_complex_float128_type_node;

/* logical_type_node is the Fortran LOGICAL type of default kind.  In
   addition to uses mandated by the Fortran standard, also prefer it
   for compiler generated temporary variables, is it avoids some minor
   issues with boolean_type_node (the C/C++ _Bool/bool). Namely:
   - On x86, partial register stalls with 8/16 bit register access,
     and length prefix changes.
   - On s390 there is a compare with immediate and jump instruction,
     but it works only with 32-bit quantities and not 8-bit such as
     boolean_type_node.
*/
extern GTY(()) tree logical_type_node;
extern GTY(()) tree logical_true_node;
extern GTY(()) tree logical_false_node;

/* This is the type used to hold the lengths of character variables.
   It must be the same as the corresponding definition in gfortran.h.  */
extern GTY(()) tree gfc_charlen_type_node;


/* The following flags give us information on the correspondence of
   real (and complex) kinds with C floating-point types long double
   and _Float128.  */
extern bool gfc_real16_is_float128;

/* True if IEC 60559 *f128 APIs should be used for _Float128 rather than
   libquadmath *q APIs.  */
extern bool gfc_real16_use_iec_60559;

enum gfc_packed {
  PACKED_NO = 0,
  PACKED_PARTIAL,
  PACKED_FULL,
  PACKED_STATIC
};

/* trans-types.cc */
void gfc_init_kinds (void);
void gfc_init_types (void);
void gfc_init_c_interop_kinds (void);

tree get_dtype_type_node (void);
tree gfc_get_int_type (int);
tree gfc_get_real_type (int);
tree gfc_get_complex_type (int);
tree gfc_get_logical_type (int);
tree gfc_get_char_type (int);
tree gfc_get_pchar_type (int);
tree gfc_get_character_type (int, gfc_charlen *);
tree gfc_get_character_type_len (int, tree);
tree gfc_get_character_type_len_for_eltype (tree, tree);

tree gfc_sym_type (gfc_symbol *, bool is_bind_c_arg = false);
tree gfc_get_cfi_type (int dimen, bool restricted);
tree gfc_typenode_for_spec (gfc_typespec *, int c = 0);
int gfc_copy_dt_decls_ifequal (gfc_symbol *, gfc_symbol *, bool);

tree gfc_get_function_type (gfc_symbol *, gfc_actual_arglist *args = NULL,
			    const char *fnspec = NULL);

tree gfc_type_for_size (unsigned, int);
tree gfc_type_for_mode (machine_mode, int);
tree gfc_build_uint_type (int);

tree gfc_get_element_type (tree);
tree gfc_get_array_type_bounds (tree, int, int, tree *, tree *, int,
				enum gfc_array_kind, bool);
tree gfc_get_nodesc_array_type (tree, gfc_array_spec *, gfc_packed, bool);

/* Add a field of given name and type to a UNION_TYPE or RECORD_TYPE.  */
tree gfc_add_field_to_struct (tree, tree, tree, tree **);

/* Layout and output debugging info for a type.  */
void gfc_finish_type (tree);

/* Some functions have an extra parameter for the return value.  */
int gfc_return_by_reference (gfc_symbol *);

/* Returns true if the array sym does not require a descriptor.  */
int gfc_is_nodesc_array (gfc_symbol *);

/* Return the DTYPE for an array.  */
tree gfc_get_dtype_rank_type (int, tree);
tree gfc_get_dtype (tree, int *rank = NULL);

tree gfc_get_caf_vector_type (int dim);
tree gfc_get_caf_reference_type ();

#endif
