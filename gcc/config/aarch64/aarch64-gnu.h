/* Definitions for AArch64 running GNU/Hurd.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_GNU_H
#define GCC_AARCH64_GNU_H

#define GNU_USER_DYNAMIC_LINKER "/lib/ld-aarch64%{mbig-endian:_be}%{mabi=ilp32:_ilp32}.so.1"

#define CPP_SPEC "%{pthread:-D_REENTRANT}"

#define GNU_TARGET_LINK_SPEC  "%{h*}		\
   %{static:-Bstatic}				\
   %{shared:-shared}				\
   %{symbolic:-Bsymbolic}			\
   %{!static:%{!static-pie:			\
     %{rdynamic:-export-dynamic}		\
     %{!shared:-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}}} \
   %{static-pie:-Bstatic -pie --no-dynamic-linker -z text} \
   -X						\
   %{mbig-endian:-EB} %{mlittle-endian:-EL}     \
   -maarch64gnu%{mabi=ilp32:32}%{mbig-endian:b}"

#ifndef CC1_SPEC
# define CC1_SPEC AARCH64_ERRATA_COMPILE_SPEC
#endif

#ifndef CC1PLUS_SPEC
# define CC1PLUS_SPEC AARCH64_ERRATA_COMPILE_SPEC
#endif

#define LINK_SPEC GNU_TARGET_LINK_SPEC AARCH64_ERRATA_LINK_SPEC

#define GNU_USER_TARGET_MATHFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:%{!shared:crtfastmath.o%s}}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC   \
  GNU_USER_TARGET_MATHFILE_SPEC " " \
  GNU_USER_TARGET_ENDFILE_SPEC

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	GNU_USER_TARGET_OS_CPP_BUILTINS();	\
    }						\
  while (0)

#define TARGET_ASM_FILE_END aarch64_file_end_indicate_exec_stack

/* Uninitialized common symbols in non-PIE executables, even with
   strong definitions in dependent shared libraries, will resolve
   to COPY relocated symbol in the executable.  See PR65780.  */
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P default_binds_local_p_2

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#endif  /* GCC_AARCH64_GNU_H */
