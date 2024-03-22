/* Definitions for ia64-linux target.

Copyright (C) 2000-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* This is for -profile to use -lc_p instead of -lc.  */
#undef CC1_SPEC
#define CC1_SPEC "%{profile:-p} %{G*}"

/* Target OS builtins.  */
#define TARGET_OS_CPP_BUILTINS()		\
do {						\
	GNU_USER_TARGET_OS_CPP_BUILTINS();	\
	builtin_define("_LONGLONG");		\
} while (0)

/* Need to override linux.h STARTFILE_SPEC, since it has crtbeginT.o in.  */
#undef STARTFILE_SPEC
#ifdef HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}}\
   crti.o%s %{shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;:crt1.o%s}}\
   crti.o%s %{shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#endif

/* Similar to standard Linux, but adding -ffast-math support.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:%{!shared:crtfastmath.o%s}} \
   %{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

/* Define this for shared library support because it isn't in the main
   linux.h file.  */

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux-ia64.so.2"

#undef LINK_SPEC
#define LINK_SPEC "\
  %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker " GNU_USER_DYNAMIC_LINKER "} \
      %{static:-static}}"

#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

#define JMP_BUF_SIZE  76

/* Override linux.h LINK_EH_SPEC definition.
   Signalize that because we have fde-glibc, we don't need all C shared libs
   linked against -lgcc_s.  */
#undef LINK_EH_SPEC
#define LINK_EH_SPEC ""

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS ia64_soft_fp_init_libfuncs

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1
