/* Definitions for StrongARM running FreeBSD using the ELF format
   Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by David E. O'Brien <obrien@FreeBSD.org> and BSDi.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC FBSD_CPP_SPEC


/************************[  Target stuff  ]***********************************/

/* Define the actual types of some ANSI-mandated types.  
   Needs to agree with <machine/ansi.h>.  GCC defaults come from c-decl.c,
   c-common.c, and config/<arch>/<arch>.h.  */

/* arm.h gets this wrong for FreeBSD.  We use the GCC defaults instead.  */

#undef  SIZE_TYPE
#define SIZE_TYPE	"unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE	"int"

/* We use the GCC defaults here.  */
#undef WCHAR_TYPE

#undef  WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef  SUBTARGET_CPU_DEFAULT
#define SUBTARGET_CPU_DEFAULT	TARGET_CPU_strongarm

#undef  ARM_OS_NAME
#define ARM_OS_NAME "FreeBSD"

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (FreeBSD/StrongARM ELF)");
