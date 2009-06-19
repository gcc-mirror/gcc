/* Simulate storage of variables into target memory, header.
   Copyright (C) 2007, 2008
   Free Software Foundation, Inc.
   Contributed by Paul Thomas and Brooks Moses

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

#ifndef GFC_TARGET_MEMORY_H
#define GFC_TARGET_MEMORY_H

#include "gfortran.h"

/* Convert a BOZ to REAL or COMPLEX.  */
bool gfc_convert_boz (gfc_expr *, gfc_typespec *);

/* Return the size of an expression in its target representation.  */
size_t gfc_target_expr_size (gfc_expr *);

/* Write a constant expression in binary form to a target buffer.  */
int gfc_encode_character (int, int, const gfc_char_t *, unsigned char *,
			  size_t);
int gfc_target_encode_expr (gfc_expr *, unsigned char *, size_t);

/* Read a target buffer into a constant expression.  */

int gfc_interpret_integer (int, unsigned char *, size_t, mpz_t);
int gfc_interpret_float (int, unsigned char *, size_t, mpfr_t);
#ifdef HAVE_mpc
int gfc_interpret_complex (int, unsigned char *, size_t, mpc_t);
#else
int gfc_interpret_complex (int, unsigned char *, size_t, mpfr_t, mpfr_t);
#endif
int gfc_interpret_logical (int, unsigned char *, size_t, int *);
int gfc_interpret_character (unsigned char *, size_t, gfc_expr *);
int gfc_interpret_derived (unsigned char *, size_t, gfc_expr *);
int gfc_target_interpret_expr (unsigned char *, size_t, gfc_expr *);

/* Merge overlapping equivalence initializers for trans-common.c. */
size_t gfc_merge_initializers (gfc_typespec, gfc_expr *,
			       unsigned char *, unsigned char *,
			       size_t);

#endif /* GFC_TARGET_MEMORY_H  */
