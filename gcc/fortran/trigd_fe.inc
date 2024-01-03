

/* Stub for defining degree-valued trigonemetric functions using MPFR.
   Copyright (C) 2000-2024 Free Software Foundation, Inc.
   Contributed by Fritz Reese <foreese@gcc.gnu.org>
   and Steven G. Kargl <kargl@gcc.gnu.org>

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

#define FTYPE mpfr_t
#define RETTYPE void
#define RETURN(x) do { } while (0)
#define ITYPE mpz_t

#define ISFINITE(x) mpfr_number_p(x)
#define D2R(x) deg2rad(x)

#define ENABLE_SIND
#define ENABLE_COSD
#define ENABLE_TAND

#define SIND simplify_sind
#define COSD simplify_cosd
#define TAND simplify_tand

/* cosd(30) === sqrt(3) / 2.  */
#define SET_COSD30(x) do { \
    mpfr_set_ui (x, 3, GFC_RND_MODE); \
    mpfr_sqrt (x, x, GFC_RND_MODE); \
    mpfr_div_ui (x, x, 2, GFC_RND_MODE); \
  } while (0)

static RETTYPE SIND (FTYPE);
static RETTYPE COSD (FTYPE);
static RETTYPE TAND (FTYPE);

#include "../../libgfortran/intrinsics/trigd.inc"

/* vim: set ft=c: */
