/* IA32 VxWorks AE target definitions for GNU compiler.
   Copyright (C) 2005-2013 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* On VxWorks AE, we only want SIMNT.  */
#undef VXWORKS_CPU_DEFINE
#define VXWORKS_CPU_DEFINE()			\
  do						\
    builtin_define ("CPU=SIMNT");		\
  while (0)

#undef  ASM_SPEC
#define ASM_SPEC ""

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
