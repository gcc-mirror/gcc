/* Definitions of target machine for GCC,
   for SPARC running in an embedded environment using the ELF file format.
   Copyright (C) 1997 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crti.o%s crtbegin.o%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   crtend.o%s crtn.o%s"

/* Use the default.  */
#undef LINK_SPEC

/* Don't set the target flags, this is done by the linker script */
#undef LIB_SPEC
#define LIB_SPEC ""

/* FIXME: until fixed */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* This solaris2 define does not apply.  */
#undef STDC_0_IN_SYSTEM_HEADERS

/* We don't want to use the Solaris2 specific long long int conversion
   routines or 64-bit integer multiply and divide routines.  */
#undef SUN_CONVERSION_LIBFUNCS
#define SUN_CONVERSION_LIBFUNCS 0

#undef SUN_INTEGER_MULTIPLY_64
#define SUN_INTEGER_MULTIPLY_64 0
