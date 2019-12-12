/* Definitions of target machine for GCC.  m68k/ColdFire based uClinux system
   using ELF objects with special linker post-processing to produce FLAT
   executables.

   Copyright (C) 2003-2019 Free Software Foundation, Inc.

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

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
"%{mshared-library-id=0|!mshared-library-id=*: crt1.o%s ;: Scrt1.o%s} \
 crti.o%s crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* Override the default LIB_SPEC from gcc.c.  We don't currently support
   profiling, or libg.a.  */
#undef LIB_SPEC
#define LIB_SPEC \
"%{mid-shared-library:%{!static-libc:-R libc.gdb%s}} %{pthread:-lpthread} -lc"

/* Default to using -elf2flt with no options.  */
#undef LINK_SPEC
#define LINK_SPEC \
"%{!elf2flt*:-elf2flt} \
 %{mid-shared-library: \
   %{mshared-library-id=*:-shared-lib-id %*;:-shared-lib-id 0}}"

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()				\
  do								\
    {								\
      GNU_USER_TARGET_OS_CPP_BUILTINS ();			\
      builtin_define ("__uClinux__");				\
      if (TARGET_ID_SHARED_LIBRARY)				\
	{							\
	  builtin_define ("__ID_SHARED_LIBRARY__");		\
	  /* Shared libraries and executables do not share	\
	     typeinfo names.  */				\
	  builtin_define ("__GXX_MERGED_TYPEINFO_NAMES=0");	\
	  builtin_define ("__GXX_TYPEINFO_EQUALITY_INLINE=0");	\
	}							\
    }								\
  while (0)

/* -msep-data is the default PIC mode on this target.  */
#define DRIVER_SELF_SPECS \
  "%{" FPIE_OR_FPIC_SPEC ":%{!msep-data:%{!mid-shared-library: -msep-data}}}"

/* The uclinux binary format relies on relocations against a segment being
   within that segment.  Conservatively apply this rule to individual
   sections.  */
#undef M68K_OFFSETS_MUST_BE_WITHIN_SECTIONS_P
#define M68K_OFFSETS_MUST_BE_WITHIN_SECTIONS_P 1
