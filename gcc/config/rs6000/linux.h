/* Definitions of target machine for GNU compiler,
   for PowerPC machines running Linux.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
   2004, 2005, 2006, 2007, 2010, 2011 Free Software Foundation, Inc.
   Contributed by Michael Meissner (meissner@cygnus.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Linux doesn't support saving and restoring 64-bit regs in a 32-bit
   process.  */
#define OS_MISSING_POWERPC64 1

/* We use glibc _mcount for profiling.  */
#define NO_PROFILE_COUNTERS 1

#ifdef SINGLE_LIBC
#define OPTION_GLIBC  (DEFAULT_LIBC == LIBC_GLIBC)
#else
#define OPTION_GLIBC  (linux_libc == LIBC_GLIBC)
#endif

/* glibc has float and long double forms of math functions.  */
#undef  TARGET_C99_FUNCTIONS
#define TARGET_C99_FUNCTIONS (OPTION_GLIBC)

/* Whether we have sincos that follows the GNU extension.  */
#undef  TARGET_HAS_SINCOS
#define TARGET_HAS_SINCOS (OPTION_GLIBC)

#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("PPC");		\
      builtin_define_std ("powerpc");		\
      builtin_assert ("cpu=powerpc");		\
      builtin_assert ("machine=powerpc");	\
      TARGET_OS_SYSV_CPP_BUILTINS ();		\
    }						\
  while (0)

#undef	CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "%(cpp_os_linux)"

/* The GNU C++ standard library currently requires _GNU_SOURCE being
   defined on glibc-based systems. This temporary hack accomplishes this,
   it should go away as soon as libstdc++-v3 has a real fix.  */
#undef  CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#undef  LINK_SHLIB_SPEC
#define LINK_SHLIB_SPEC "%{shared:-shared} %{!shared: %{static:-static}}"

#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC "%(lib_linux)"

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC "%(startfile_linux)"

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC "%(endfile_linux)"

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC "%(link_start_linux)"

#undef	LINK_OS_DEFAULT_SPEC
#define LINK_OS_DEFAULT_SPEC "%(link_os_linux)"

#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"

/* Use --as-needed -lgcc_s for eh support.  */
#ifdef HAVE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 1
#endif

/* Override rs6000.h definition.  */
#undef  ASM_APP_ON
#define ASM_APP_ON "#APP\n"

/* Override rs6000.h definition.  */
#undef  ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* For backward compatibility, we must continue to use the AIX
   structure return convention.  */
#undef  DRAFT_V4_STRUCT_RET
#define DRAFT_V4_STRUCT_RET 1

/* We are 32-bit all the time, so optimize a little.  */
#undef TARGET_64BIT
#define TARGET_64BIT 0
 
/* We don't need to generate entries in .fixup, except when
   -mrelocatable or -mrelocatable-lib is given.  */
#undef RELOCATABLE_NEEDS_FIXUP
#define RELOCATABLE_NEEDS_FIXUP \
  (target_flags & target_flags_explicit & MASK_RELOCATABLE)

#define TARGET_POSIX_IO

#ifdef TARGET_LIBC_PROVIDES_SSP
/* ppc32 glibc provides __stack_chk_guard in -0x7008(2).  */
#define TARGET_THREAD_SSP_OFFSET	-0x7008
#endif

#define POWERPC_LINUX

/* ppc linux has 128-bit long double support in glibc 2.4 and later.  */
#ifdef TARGET_DEFAULT_LONG_DOUBLE_128
#define RS6000_DEFAULT_LONG_DOUBLE_SIZE 128
#endif

/* Static stack checking is supported by means of probes.  */
#define STACK_CHECK_STATIC_BUILTIN 1
