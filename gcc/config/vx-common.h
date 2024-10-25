/* Target-independent configuration for VxWorks and VxWorks AE.
   Copyright (C) 2005-2024 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.

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

/* ------------------------- Common SPEC strings -------------------------  */

/* Most of these will probably be overridden by subsequent headers.  We
   undefine them here just in case, and define VXWORKS_ versions of each,
   to be used in port-specific vxworks.h.  */

/* REAL_LIBGCC_SPEC needs to be used since the non-static option is not
   handled in gcc.cc.  */
#undef REAL_LIBGCC_SPEC
#define REAL_LIBGCC_SPEC VXWORKS_LIBGCC_SPEC
#undef STARTFILE_SPEC
#undef ENDFILE_SPEC

/* Most of these macros are overridden in "config/vxworks.h" or
   "config/vxworksae.h" and are here merely for documentation
   purposes.  */
#define VXWORKS_ADDITIONAL_CPP_SPEC ""
#define	VXWORKS_LIB_SPEC ""
#define VXWORKS_LINK_SPEC ""
#define VXWORKS_LIBGCC_SPEC ""
#define	VXWORKS_STARTFILE_SPEC ""
#define VXWORKS_ENDFILE_SPEC ""
#define VXWORKS_CC1_SPEC ""

/* ----------------------- Common type descriptions -----------------------  */

/* Regardless of the target architecture, VxWorks uses a signed 32bit
   integer for wchar_t starting with vx7 SR06xx.  An unsigned short
   otherwise.  */
#if TARGET_VXWORKS7

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32
#undef WCHAR_TYPE
#define WCHAR_TYPE (TARGET_VXWORKS64 ? "int" : "long int")

#else

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16
#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#endif

/* The VxWorks headers base wint_t on the definitions used for wchar_t.
   Do the same here to make sure they remain in sync, in case WCHAR_TYPE
   gets redefined for a specific CPU architecture.  */
#undef WINT_TYPE_SIZE
#define WINT_TYPE_SIZE WCHAR_TYPE_SIZE
#undef WINT_TYPE
#define WINT_TYPE "wchar_t"

/* ---------------------- Debug and unwind info formats ------------------  */

/* Dwarf2 unwind info is supported, unless overriden by a request for a target
   specific format.  Always #define DWARF2_UNWIND_INFO to prevent defaults.h
   from picking a possibly different value.

   Taking care of this here allows using DWARF2_UNWIND_INFO in #if conditions
   from the common config/vxworks.h files, included before the cpu
   specializations.  Unlike with conditions used in C expressions, where the
   definitions which matter are those at the expression expansion point, use
   in #if constructs requires an accurate definition of the operands at the
   #if point.  Since <cpu>/vxworks.h. is typically included after
   config/vxworks.h, #if expressions in the latter can't rely on possible
   redefinitions in the former.  */
#undef DWARF2_UNWIND_INFO
#if ARM_UNWIND_INFO
#define DWARF2_UNWIND_INFO 0
#else
#define DWARF2_UNWIND_INFO 1
#endif

/* VxWorks uses DWARF2 debugging info.  */
#define DWARF2_DEBUGGING_INFO 1
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* None of these other formats is supported.  */
#undef VMS_DEBUGGING_INFO

/* ------------------------ Misc configuration bits ----------------------  */

#if !TARGET_VXWORKS7
/* VxWorks, prior to version 7, could not have dots in constructor
   labels, because it used a mutant variation of collect2 that
   generates C code instead of assembly.  Thus each constructor label
   had to be a legitimate C symbol.  */
# undef NO_DOLLAR_IN_LABEL
# define NO_DOT_IN_LABEL
#endif

/* Kernel mode doesn't have ctors/dtors, but RTP mode does.  */
#define TARGET_HAVE_CTORS_DTORS false
#define VXWORKS_OVERRIDE_OPTIONS /* empty */

/* No math library needed.  */
#define MATH_LIBRARY ""

/* No profiling.  */
#define VXWORKS_FUNCTION_PROFILER(FILE, LABELNO) do	\
{							\
  sorry ("profiler support for VxWorks");		\
} while (0)

/* We occasionally need to distinguish between the VxWorks variants.  */
#define VXWORKS_KIND_NORMAL  1
#define VXWORKS_KIND_AE      2
