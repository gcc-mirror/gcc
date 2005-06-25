/* Header for code constant translation functions
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
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

/* Returns an INT_CST.  */
tree gfc_conv_mpz_to_tree (mpz_t, int);

/* Returns a REAL_CST.  */
tree gfc_conv_mpfr_to_tree (mpfr_t, int);

/* Build a tree for a constant.  Must be an EXPR_CONSTANT gfc_expr.
   For CHARACTER literal constants, the caller still has to set the
   string length as a separate operation.  */
tree gfc_conv_constant_to_tree (gfc_expr *);

/* Like gfc_conv_noncharacter_constant, but works on simplified expression
   structures.  Also sets the length of CHARACTER strings in the gfc_se.  */
void gfc_conv_constant (gfc_se *, gfc_expr *);

tree gfc_build_string_const (int, const char *);
tree gfc_build_cstring_const (const char *);

/* Translate a string constant for a static initializer.  */
tree gfc_conv_string_init (tree, gfc_expr *);

/* Create a tree node for the string length if it is constant.  */
void gfc_conv_const_charlen (gfc_charlen *);

/* Initialize the nodes for constants.  */
void gfc_init_constants (void);

/* Build a constant with given type from an int_cst.  */
tree gfc_build_const (tree, tree);

/* String constants.  */
extern GTY(()) tree gfc_strconst_current_filename;
extern GTY(()) tree gfc_strconst_bounds;
extern GTY(()) tree gfc_strconst_fault;
extern GTY(()) tree gfc_strconst_wrong_return;

/* Integer constants 0..GFC_MAX_DIMENSIONS.  */
extern GTY(()) tree gfc_rank_cst[GFC_MAX_DIMENSIONS + 1];

#define gfc_index_zero_node gfc_rank_cst[0]
#define gfc_index_one_node gfc_rank_cst[1]
