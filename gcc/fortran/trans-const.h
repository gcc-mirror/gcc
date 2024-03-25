/* Header for code constant translation functions
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
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

/* Converts between INT_CST and GMP integer representations.  */
tree gfc_conv_mpz_to_tree (mpz_t, int);
tree gfc_conv_mpz_to_tree_type (mpz_t, const tree);
void gfc_conv_tree_to_mpz (mpz_t, tree);

/* Converts between REAL_CST and MPFR floating-point representations.  */
tree gfc_conv_mpfr_to_tree (mpfr_t, int, int);
void gfc_conv_tree_to_mpfr (mpfr_ptr, tree);

/* Build a tree containing a real infinity (or HUGE if infinities are
   not supported for the given type.  */
tree gfc_build_inf_or_huge (tree, int);

/* Build a tree containing a NaN for the given type, with significand
   specified by second argument.  */
tree gfc_build_nan (tree, const char *);

/* Build a tree for a constant.  Must be an EXPR_CONSTANT gfc_expr.
   For CHARACTER literal constants, the caller still has to set the
   string length as a separate operation.  */
tree gfc_conv_constant_to_tree (gfc_expr *);

/* Like gfc_conv_noncharacter_constant, but works on simplified expression
   structures.  Also sets the length of CHARACTER strings in the gfc_se.  */
void gfc_conv_constant (gfc_se *, gfc_expr *);

tree gfc_build_string_const (size_t, const char *);
tree gfc_build_wide_string_const (int, size_t, const gfc_char_t *);
tree gfc_build_cstring_const (const char *);
tree gfc_build_localized_cstring_const (const char *);

/* Translate a string constant for a static initializer.  */
tree gfc_conv_string_init (tree, gfc_expr *);

/* Create a tree node for the string length if it is constant.  */
void gfc_conv_const_charlen (gfc_charlen *);

/* Initialize the nodes for constants.  */
void gfc_init_constants (void);

/* Build a constant with given type from an int_cst.  */
tree gfc_build_const (tree, tree);

/* Integer constants 0..GFC_MAX_DIMENSIONS.  */
extern GTY(()) tree gfc_rank_cst[GFC_MAX_DIMENSIONS + 1];

#define gfc_index_zero_node gfc_rank_cst[0]
#define gfc_index_one_node gfc_rank_cst[1]
