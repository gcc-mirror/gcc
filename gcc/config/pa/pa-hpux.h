/* Definitions of target machine for GNU compiler, for HP-UX.
   Copyright (C) 1991-2025 Free Software Foundation, Inc.

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

/* HP-UX UNIX features.  */
#undef TARGET_HPUX
#define TARGET_HPUX 1

#undef HPUX_LONG_DOUBLE_LIBRARY
#define HPUX_LONG_DOUBLE_LIBRARY 1

#undef TARGET_DEFAULT
#define TARGET_DEFAULT 0

/* Make GCC agree with types.h.  */
#undef SIZE_TYPE
#undef PTRDIFF_TYPE

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"

#undef PA_LONG_DOUBLE_TYPE_SIZE
#define PA_LONG_DOUBLE_TYPE_SIZE 128
#define FLOAT_LIB_COMPARE_RETURNS_BOOL(MODE, COMPARISON) ((MODE) == TFmode)

/* GCC always defines __STDC__.  HP C++ compilers don't define it.  This
   causes trouble when sys/stdsyms.h is included.  As a work around,
   we define __STDC_EXT__.  A similar situation exists with respect to
   the definition of __cplusplus.  We define _INCLUDE_LONGLONG
   to prevent nlist.h from defining __STDC_32_MODE__ (no longlong
   support).  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()				\
  do								\
    {								\
	builtin_assert ("system=hpux");				\
	builtin_assert ("system=unix");				\
	builtin_define ("__hp9000s800");			\
	builtin_define ("__hp9000s800__");			\
	builtin_define ("__hp9k8");				\
	builtin_define ("__hp9k8__");				\
	builtin_define ("__hpux");				\
	builtin_define ("__hpux__");				\
	builtin_define ("__unix");				\
	builtin_define ("__unix__");				\
	builtin_define ("__STDC_EXT__");			\
	if (c_dialect_cxx ())					\
	  {							\
	    builtin_define ("_HPUX_SOURCE");			\
	    builtin_define ("_INCLUDE_LONGLONG");		\
	  }							\
	else if (!flag_iso)					\
	  {							\
	    builtin_define ("_HPUX_SOURCE");			\
	    if (preprocessing_trad_p ())			\
	      {							\
		builtin_define ("hp9000s800");			\
		builtin_define ("hp9k8");			\
		builtin_define ("hppa");			\
		builtin_define ("hpux");			\
		builtin_define ("unix");			\
		builtin_define ("__CLASSIC_C__");		\
		builtin_define ("_PWB");			\
		builtin_define ("PWB");				\
	      }							\
	  }							\
	if (TARGET_SIO)						\
	  builtin_define ("_SIO");				\
	else							\
	  {							\
	    builtin_define ("__hp9000s700");			\
	    builtin_define ("__hp9000s700__");			\
	    builtin_define ("_WSIO");				\
	  }							\
    }								\
  while (0)

/* Like the default, except no -lg.  */
#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{!p:%{!pg:-lc}}%{p: -L/lib/libp/ -lc}%{pg: -L/lib/libp/ -lc}}"

#undef LINK_SPEC
#if ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_PA_11)
#define LINK_SPEC \
  "%{!mpa-risc-1-0:%{!march=1.0:%{static:-L/lib/pa1.1 -L/usr/lib/pa1.1 }}}%{mlinker-opt:-O} %{!shared:-u main} %{static:-a archive} %{g*:-a archive} %{shared:-b}"
#else
#define LINK_SPEC \
  "%{mlinker-opt:-O} %{!shared:-u main} %{static:-a archive} %{g*:-a archive} %{shared:-b}"
#endif

/* hpux11 and earlier don't have fputc_unlocked, so we must inhibit the
   transformation of fputs_unlocked and fprintf_unlocked to fputc_unlocked.  */
#define DONT_HAVE_FPUTC_UNLOCKED

/* We want the entry value of SP saved in the frame marker for
   compatibility with the HP-UX unwind library.  */
#undef TARGET_HPUX_UNWIND_LIBRARY
#define TARGET_HPUX_UNWIND_LIBRARY 1

#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION no_c99_libc_has_function

/* Assume we have libatomic if sync libcalls are disabled.  */
#undef TARGET_HAVE_LIBATOMIC
#define TARGET_HAVE_LIBATOMIC (!flag_sync_libcalls)

/* The SYNC operations are implemented as library functions, not
   INSN patterns.  As a result, the HAVE defines for the patterns are
   not defined.  We need to define them to generate the corresponding
   __GCC_HAVE_SYNC_COMPARE_AND_SWAP_* and __GCC_ATOMIC_*_LOCK_FREE
   defines.  */
#define HAVE_sync_compare_and_swapqi (flag_sync_libcalls)
#define HAVE_sync_compare_and_swaphi (flag_sync_libcalls)
#define HAVE_sync_compare_and_swapsi (flag_sync_libcalls)
#define HAVE_sync_compare_and_swapdi (flag_sync_libcalls)
