/* Definitions of target machine for GNU compiler,
   for Alpha Linux-based GNU systems.
   Copyright (C) 1996-2023 Free Software Foundation, Inc.
   Contributed by Richard Henderson.

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

#define TARGET_OS_CPP_BUILTINS()				\
    do {							\
	builtin_define ("__gnu_linux__");			\
	builtin_define ("_LONGLONG");				\
	builtin_define_std ("linux");				\
	builtin_define_std ("unix");				\
	builtin_assert ("system=linux");			\
	builtin_assert ("system=unix");				\
	builtin_assert ("system=posix");			\
	/* The GNU C++ standard library requires this.  */	\
	if (c_dialect_cxx ())					\
	  builtin_define ("_GNU_SOURCE");			\
    } while (0)

#undef LIB_SPEC
#define LIB_SPEC \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared: %{profile:-lc_p}%{!profile:-lc}}"

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

/* Show that we need a GP when profiling.  */
#undef TARGET_PROFILING_NEEDS_GP
#define TARGET_PROFILING_NEEDS_GP 1

/* Don't care about faults in the prologue.  */
#undef TARGET_CAN_FAULT_IN_PROLOGUE
#define TARGET_CAN_FAULT_IN_PROLOGUE 1

/* OS fixes up EV5 data fault on prefetch.  */
#undef TARGET_FIXUP_EV5_PREFETCH
#define TARGET_FIXUP_EV5_PREFETCH 1

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#ifdef SINGLE_LIBC
#define OPTION_GLIBC_P(opts)	(DEFAULT_LIBC == LIBC_GLIBC)
#define OPTION_UCLIBC_P(opts)	(DEFAULT_LIBC == LIBC_UCLIBC)
#define OPTION_BIONIC_P(opts)	(DEFAULT_LIBC == LIBC_BIONIC)
#undef OPTION_MUSL_P
#define OPTION_MUSL_P(opts)	(DEFAULT_LIBC == LIBC_MUSL)
#else
#define OPTION_GLIBC_P(opts)	((opts)->x_linux_libc == LIBC_GLIBC)
#define OPTION_UCLIBC_P(opts)	((opts)->x_linux_libc == LIBC_UCLIBC)
#define OPTION_BIONIC_P(opts)	((opts)->x_linux_libc == LIBC_BIONIC)
#undef OPTION_MUSL_P
#define OPTION_MUSL_P(opts)	((opts)->x_linux_libc == LIBC_MUSL)
#endif
#define OPTION_GLIBC		OPTION_GLIBC_P (&global_options)
#define OPTION_UCLIBC		OPTION_UCLIBC_P (&global_options)
#define OPTION_BIONIC		OPTION_BIONIC_P (&global_options)
#undef OPTION_MUSL
#define OPTION_MUSL		OPTION_MUSL_P (&global_options)

/* Determine what functions are present at the runtime;
   this includes full c99 runtime and sincos.  */
#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION linux_libc_has_function

#define TARGET_POSIX_IO

/* Provide a STARTFILE_SPEC appropriate for ELF.  Here we add the
   (even more) magical crtbegin.o file which provides part of the
   support for getting C++ file-scope static object constructed
   before entering `main'.  */

#undef	STARTFILE_SPEC
#ifdef HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}}\
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p:gcrt1.o%s;:crt1.o%s}}\
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#endif

/* Provide a ENDFILE_SPEC appropriate for ELF.  Here we tack on the
   magical crtend.o file which provides part of the support for
   getting C++ file-scope static object constructed before entering
   `main', followed by a normal ELF "finalizer" file, `crtn.o'.  */

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:%{!shared:crtfastmath.o%s}} \
   %{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static|static-pie:--start-group} %G %{!nolibc:%L} \
   %{static|static-pie:--end-group}%{!static:%{!static-pie:%G}}"

/* Use --as-needed -lgcc_s for eh support.  */
#ifdef HAVE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 1
#endif

/* Define if long doubles should be mangled as 'g'.  */
#define TARGET_ALTERNATE_LONG_DOUBLE_MANGLING

/* -mcpu=native handling only makes sense with compiler running on
   an Alpha chip.  */
#if defined(__alpha__) || defined(__alpha)
extern const char *host_detect_local_cpu (int argc, const char **argv);
# define EXTRA_SPEC_FUNCTIONS						\
  { "local_cpu_detect", host_detect_local_cpu },

# define MCPU_MTUNE_NATIVE_SPECS					\
   " %{mcpu=native:%<mcpu=native %:local_cpu_detect(cpu)}"		\
   " %{mtune=native:%<mtune=native %:local_cpu_detect(tune)}"
#else
# define MCPU_MTUNE_NATIVE_SPECS ""
#endif

#define DRIVER_SELF_SPECS MCPU_MTUNE_NATIVE_SPECS
