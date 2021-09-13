/* Compiler arithmetic header.
   Copyright (C) 2000-2021 Free Software Foundation, Inc.
   Contributed by Steven Bosscher

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

#ifndef GFC_ARITH_H
#define GFC_ARITH_H

/* MPFR also does not have the conversion of a mpfr_t to a mpz_t, so declare
   a function for this as well.  */

void gfc_mpfr_to_mpz (mpz_t, mpfr_t, locus *);
void gfc_set_model_kind (int);
void gfc_set_model (mpfr_t);

/* Make sure a gfc_expr expression is within its allowed range.  Checks
   for overflow and underflow.  */
arith gfc_range_check (gfc_expr *);

int gfc_compare_expr (gfc_expr *, gfc_expr *, gfc_intrinsic_op);
int gfc_compare_string (gfc_expr *, gfc_expr *);
int gfc_compare_with_Cstring (gfc_expr *, const char *, bool);


/* Constant folding for gfc_expr trees.  */
gfc_expr *gfc_parentheses (gfc_expr * op);
gfc_expr *gfc_uplus (gfc_expr * op);
gfc_expr *gfc_uminus (gfc_expr * op);
gfc_expr *gfc_add (gfc_expr *, gfc_expr *);
gfc_expr *gfc_subtract (gfc_expr *, gfc_expr *);
gfc_expr *gfc_multiply (gfc_expr *, gfc_expr *);
gfc_expr *gfc_divide (gfc_expr *, gfc_expr *);
gfc_expr *gfc_power (gfc_expr *, gfc_expr *);
gfc_expr *gfc_concat (gfc_expr *, gfc_expr *);
gfc_expr *gfc_and (gfc_expr *, gfc_expr *);
gfc_expr *gfc_or (gfc_expr *, gfc_expr *);
gfc_expr *gfc_not (gfc_expr *);
gfc_expr *gfc_eqv (gfc_expr *, gfc_expr *);
gfc_expr *gfc_neqv (gfc_expr *, gfc_expr *);
gfc_expr *gfc_eq (gfc_expr *, gfc_expr *, gfc_intrinsic_op);
gfc_expr *gfc_ne (gfc_expr *, gfc_expr *, gfc_intrinsic_op);
gfc_expr *gfc_gt (gfc_expr *, gfc_expr *, gfc_intrinsic_op);
gfc_expr *gfc_ge (gfc_expr *, gfc_expr *, gfc_intrinsic_op);
gfc_expr *gfc_lt (gfc_expr *, gfc_expr *, gfc_intrinsic_op);
gfc_expr *gfc_le (gfc_expr *, gfc_expr *, gfc_intrinsic_op);

/* Convert a constant of one kind to another kind.  */
gfc_expr *gfc_int2int (gfc_expr *, int);
gfc_expr *gfc_int2real (gfc_expr *, int);
gfc_expr *gfc_int2complex (gfc_expr *, int);
gfc_expr *gfc_real2int (gfc_expr *, int);
gfc_expr *gfc_real2real (gfc_expr *, int);
gfc_expr *gfc_real2complex (gfc_expr *, int);
gfc_expr *gfc_complex2int (gfc_expr *, int);
gfc_expr *gfc_complex2real (gfc_expr *, int);
gfc_expr *gfc_complex2complex (gfc_expr *, int);
gfc_expr *gfc_log2log (gfc_expr *, int);
gfc_expr *gfc_log2int (gfc_expr *, int);
gfc_expr *gfc_int2log (gfc_expr *, int);
gfc_expr *gfc_hollerith2int (gfc_expr *, int);
gfc_expr *gfc_hollerith2real (gfc_expr *, int);
gfc_expr *gfc_hollerith2complex (gfc_expr *, int);
gfc_expr *gfc_hollerith2character (gfc_expr *, int);
gfc_expr *gfc_hollerith2logical (gfc_expr *, int);
gfc_expr *gfc_character2int (gfc_expr *, int);
gfc_expr *gfc_character2real (gfc_expr *, int);
gfc_expr *gfc_character2complex (gfc_expr *, int);
gfc_expr *gfc_character2character (gfc_expr *, int);
gfc_expr *gfc_character2logical (gfc_expr *, int);

#endif /* GFC_ARITH_H  */

