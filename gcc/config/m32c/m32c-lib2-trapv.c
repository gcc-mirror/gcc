/* 16-bit trapping arithmetic routines for R8C/M16C/M32C
   Copyright (C) 2009
   Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   In addition to the permissions in the GNU General Public License,
   the Free Software Foundation gives you unlimited permission to link
   the compiled version of this file into combinations with other
   programs, and to distribute those combinations without any
   restriction coming from the use of this file.  (The General Public
   License restrictions do apply in other respects; for example, they
   cover modification of the file, and distribution when not linked
   into a combine executable.)

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* See the comment by the definition of LIBGCC2_UNITS_PER_WORD in
   m32c.h for why we are creating extra versions of some of the
   functions defined in libgcc2.c.

   Note - this file is separate from m32c-lib2.c so that the following
   functions will appear in the their object file.  This is necessary
   because they call abort() which is defined in the C library whereas
   the functions in m32c-lib2.c are completely self sufficieent.  */

#define LIBGCC2_UNITS_PER_WORD 2

#define L_mulvsi3
#define L_negvsi2
#define L_addvsi3
#define L_subvsi3

#include "libgcc2.c"
