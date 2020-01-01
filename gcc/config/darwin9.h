/* Target definitions for Darwin (Mac OS X) systems.
   Copyright (C) 2006-2020 Free Software Foundation, Inc.
   Contributed by Apple Inc.

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

/* Prefer DWARF2.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DARWIN_PREFER_DWARF

/* Since DWARF2 is default, conditions for running dsymutil are different.  */
#undef DSYMUTIL_SPEC
#define DSYMUTIL_SPEC \
   "%{!fdump=*:%{!fsyntax-only:%{!c:%{!M:%{!MM:%{!E:%{!S:\
    %{v} \
    %{g*:%{!gstabs*:%{%:debug-level-gt(0): -idsym}}}\
    %{.c|.cc|.C|.cpp|.cp|.c++|.cxx|.CPP|.m|.mm|.s|.f|.f90|.f95|.f03|.f77|.for|.F|.F90|.F95|.F03: \
    %{g*:%{!gstabs*:%{%:debug-level-gt(0): -dsym}}}}}}}}}}}"

/* Tell collect2 to run dsymutil for us as necessary.  */
#define COLLECT_RUN_DSYMUTIL 1

/* Only ask as for debug data if the debug style is stabs (since as doesn't
   yet generate dwarf.)  */

#undef  ASM_DEBUG_SPEC
#define ASM_DEBUG_SPEC  "%{g*:%{%:debug-level-gt(0):%{gstabs:--gstabs}}}"

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
  do {									\
    unsigned HOST_WIDE_INT _new_size = (SIZE);				\
    fprintf ((FILE), "\t.comm ");						\
    assemble_name ((FILE), (NAME));					\
    if (_new_size == 0) _new_size = 1;					\
    fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",		\
	     _new_size, floor_log2 ((ALIGN) / BITS_PER_UNIT));		\
  } while (0)

#undef DEF_MIN_OSX_VERSION
#define DEF_MIN_OSX_VERSION "10.5"

#undef STACK_CHECK_STATIC_BUILTIN
#define STACK_CHECK_STATIC_BUILTIN 1
