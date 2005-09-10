/* Header for Fortran 95 types backend support.
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


#ifndef GFC_BACKEND_H
#define GFC_BACKEND_H

#define GFC_DTYPE_RANK_MASK 0x07
#define GFC_DTYPE_TYPE_SHIFT 3
#define GFC_DTYPE_TYPE_MASK 0x38
#define GFC_DTYPE_SIZE_SHIFT 6

enum
{
  GFC_DTYPE_UNKNOWN = 0,
  GFC_DTYPE_INTEGER,
  GFC_DTYPE_LOGICAL,
  GFC_DTYPE_REAL,
  GFC_DTYPE_COMPLEX,
  GFC_DTYPE_DERIVED,
  GFC_DTYPE_CHARACTER
};

extern GTY(()) tree gfc_array_index_type;
extern GTY(()) tree gfc_array_range_type;
extern GTY(()) tree gfc_character1_type_node;
extern GTY(()) tree ppvoid_type_node;
extern GTY(()) tree pvoid_type_node;
extern GTY(()) tree pchar_type_node;

/* This is the type used to hold the lengths of character variables.
   It must be the same as the corresponding definition in gfortran.h.  */
/* TODO: This is still hardcoded as kind=4 in some bits of the compiler
   and runtime library.  */
extern GTY(()) tree gfc_charlen_type_node;

/* be-function.c */
void gfc_convert_function_code (gfc_namespace *);

/* trans-types.c */
void gfc_init_kinds (void);
void gfc_init_types (void);

tree gfc_get_int_type (int);
tree gfc_get_real_type (int);
tree gfc_get_complex_type (int);
tree gfc_get_logical_type (int);
tree gfc_get_character_type (int, gfc_charlen *);
tree gfc_get_character_type_len (int, tree);

tree gfc_sym_type (gfc_symbol *);
tree gfc_typenode_for_spec (gfc_typespec *);

tree gfc_get_function_type (gfc_symbol *);

tree gfc_type_for_size (unsigned, int);
tree gfc_type_for_mode (enum machine_mode, int);
tree gfc_unsigned_type (tree);
tree gfc_signed_type (tree);
tree gfc_signed_or_unsigned_type (int, tree);

tree gfc_get_element_type (tree);
tree gfc_get_array_type_bounds (tree, int, tree *, tree *, int);
tree gfc_get_nodesc_array_type (tree, gfc_array_spec *, int);

/* Add a field of given name and type to a UNION_TYPE or RECORD_TYPE.  */
tree gfc_add_field_to_struct (tree *, tree, tree, tree);

/* Layout and output debugging info for a type.  */
void gfc_finish_type (tree);

/* Some functions have an extra parameter for the return value.  */
int gfc_return_by_reference (gfc_symbol *);

/* Returns true if the array sym does not require a descriptor.  */
int gfc_is_nodesc_array (gfc_symbol *);

/* Return the DTYPE for an array.  */
tree gfc_get_dtype (tree);

#endif
