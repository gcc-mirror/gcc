/* Compiler arithmetic header.
   Copyright (C) 2000, 2001. 2002 Free Software Foundation, Inc.
   Contributed by Steven Bosscher

This file is part of GNU G95.

GNU G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#ifndef GFC_ARITH_H
#define GFC_ARITH_H

#include "gfortran.h"

/* Constants calculated during initialization.  */
extern mpf_t pi, half_pi, two_pi, e;

/* Calculate mathematically interesting functions.  */
void natural_logarithm (mpf_t *, mpf_t *);
void common_logarithm (mpf_t *, mpf_t *);
void exponential (mpf_t *, mpf_t *);
void sine (mpf_t *, mpf_t *);
void cosine (mpf_t *, mpf_t *);
void arctangent (mpf_t *, mpf_t *);
void arctangent2 (mpf_t *, mpf_t *, mpf_t *);
void hypercos (mpf_t *, mpf_t *);
void hypersine (mpf_t *, mpf_t *);

/* Return a constant result of a given type and kind, with locus.  */
gfc_expr *gfc_constant_result (bt, int, locus *);

/* Make sure a gfc_expr expression is within its allowed range.  Checks
   for overflow and underflow.  */
arith gfc_range_check (gfc_expr *);

int gfc_compare_expr (gfc_expr *, gfc_expr *);
int gfc_compare_string (gfc_expr *, gfc_expr *, const int *);

/* Constant folding for gfc_expr trees.  */
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
gfc_expr *gfc_eq (gfc_expr *, gfc_expr *);
gfc_expr *gfc_ne (gfc_expr *, gfc_expr *);
gfc_expr *gfc_gt (gfc_expr *, gfc_expr *);
gfc_expr *gfc_ge (gfc_expr *, gfc_expr *);
gfc_expr *gfc_lt (gfc_expr *, gfc_expr *);
gfc_expr *gfc_le (gfc_expr *, gfc_expr *);

/* Convert strings to literal constants.  */
gfc_expr *gfc_convert_integer (const char *, int, int, locus *);
gfc_expr *gfc_convert_real (const char *, int, locus *);
gfc_expr *gfc_convert_complex (gfc_expr *, gfc_expr *, int);

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

#endif /* GFC_ARITH_H  */

