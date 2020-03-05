/* Definitions of target machine for GNU compiler,
   for PowerPC machines running Linux.
   Copyright (C) 1996-2019 Free Software Foundation, Inc.
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
#define OPTION_UCLIBC (DEFAULT_LIBC == LIBC_UCLIBC)
#define OPTION_BIONIC (DEFAULT_LIBC == LIBC_BIONIC)
#undef OPTION_MUSL
#define OPTION_MUSL   (DEFAULT_LIBC == LIBC_MUSL)
#else
#define OPTION_GLIBC  (linux_libc == LIBC_GLIBC)
#define OPTION_UCLIBC (linux_libc == LIBC_UCLIBC)
#define OPTION_BIONIC (linux_libc == LIBC_BIONIC)
#undef OPTION_MUSL
#define OPTION_MUSL   (linux_libc == LIBC_MUSL)
#endif

/* Determine what functions are present at the runtime;
   this includes full c99 runtime and sincos.  */
#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION linux_libc_has_function

#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()			\
  do							\
    {							\
      if (strcmp (rs6000_abi_name, "linux") == 0)	\
	GNU_USER_TARGET_OS_CPP_BUILTINS();		\
      builtin_define_std ("PPC");			\
      builtin_define_std ("powerpc");			\
      builtin_assert ("cpu=powerpc");			\
      builtin_assert ("machine=powerpc");		\
      TARGET_OS_SYSV_CPP_BUILTINS ();			\
    }							\
  while (0)

#undef	CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "%(cpp_os_linux)"

#undef  LINK_SHLIB_SPEC
#define LINK_SHLIB_SPEC "%{shared:-shared} %{!shared: %{static:-static}} \
  %{static-pie:-static -pie --no-dynamic-linker -z text}"

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

#undef  DEFAULT_ASM_ENDIAN
#if (TARGET_DEFAULT & MASK_LITTLE_ENDIAN)
#define DEFAULT_ASM_ENDIAN " -mlittle"
#define LINK_OS_LINUX_EMUL ENDIAN_SELECT(" -m elf32ppclinux",	\
					 " -m elf32lppclinux",	\
					 " -m elf32lppclinux")
#else
#define DEFAULT_ASM_ENDIAN " -mbig"
#define LINK_OS_LINUX_EMUL ENDIAN_SELECT(" -m elf32ppclinux",	\
					 " -m elf32lppclinux",	\
					 " -m elf32ppclinux")
#endif

#undef LINK_OS_LINUX_SPEC
#define LINK_OS_LINUX_SPEC LINK_OS_LINUX_EMUL " %{!shared: %{!static: \
  %{!static-pie: \
    %{rdynamic:-export-dynamic} \
    -dynamic-linker " GNU_USER_DYNAMIC_LINKER "}}}"

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
  (rs6000_isa_flags & rs6000_isa_flags_explicit & OPTION_MASK_RELOCATABLE)

#undef	RS6000_ABI_NAME
#define	RS6000_ABI_NAME "linux"

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

/* Software floating point support for exceptions and rounding modes
   depends on the C library in use.  */
#undef TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P
#define TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P \
  rs6000_linux_float_exceptions_rounding_supported_p

/* Support for TARGET_ATOMIC_ASSIGN_EXPAND_FENV without FPRs depends
   on glibc 2.19 or greater.  */
#if TARGET_GLIBC_MAJOR > 2 \
  || (TARGET_GLIBC_MAJOR == 2 && TARGET_GLIBC_MINOR >= 19)
#define RS6000_GLIBC_ATOMIC_FENV 1
#endif
