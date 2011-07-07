/* Definitions for systems using the Linux kernel, with or without
   MMU, using ELF at the compiler level but possibly FLT for final
   linked executables and shared libraries in some no-MMU cases, and
   possibly with a choice of libc implementations.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2003, 2004, 2005, 2006,
   2007, 2009, 2010, 2011 Free Software Foundation, Inc.
   Contributed by Eric Youngdale.
   Modified for stabs-in-ELF by H.J. Lu (hjl@lucon.org).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* C libraries supported on Linux.  */
#ifdef SINGLE_LIBC
#define OPTION_GLIBC  (DEFAULT_LIBC == LIBC_GLIBC)
#define OPTION_UCLIBC (DEFAULT_LIBC == LIBC_UCLIBC)
#define OPTION_BIONIC (DEFAULT_LIBC == LIBC_BIONIC)
#else
#define OPTION_GLIBC  (linux_libc == LIBC_GLIBC)
#define OPTION_UCLIBC (linux_libc == LIBC_UCLIBC)
#define OPTION_BIONIC (linux_libc == LIBC_BIONIC)
#endif

#define GNU_USER_TARGET_OS_CPP_BUILTINS()			\
    do {							\
	if (OPTION_GLIBC)					\
	  builtin_define ("__gnu_linux__");			\
	builtin_define_std ("linux");				\
	builtin_define_std ("unix");				\
	builtin_assert ("system=linux");			\
	builtin_assert ("system=unix");				\
	builtin_assert ("system=posix");			\
    } while (0)

/* Determine which dynamic linker to use depending on whether GLIBC or
   uClibc or Bionic is the default C library and whether
   -muclibc or -mglibc or -mbionic has been passed to change the default.  */

#define CHOOSE_DYNAMIC_LINKER1(LIBC1, LIBC2, LIBC3, LD1, LD2, LD3)	\
  "%{" LIBC2 ":" LD2 ";:%{" LIBC3 ":" LD3 ";:" LD1 "}}"

#if DEFAULT_LIBC == LIBC_GLIBC
#define CHOOSE_DYNAMIC_LINKER(G, U, B) \
  CHOOSE_DYNAMIC_LINKER1 ("mglibc", "muclibc", "mbionic", G, U, B)
#elif DEFAULT_LIBC == LIBC_UCLIBC
#define CHOOSE_DYNAMIC_LINKER(G, U, B) \
  CHOOSE_DYNAMIC_LINKER1 ("muclibc", "mglibc", "mbionic", U, G, B)
#elif DEFAULT_LIBC == LIBC_BIONIC
#define CHOOSE_DYNAMIC_LINKER(G, U, B) \
  CHOOSE_DYNAMIC_LINKER1 ("mbionic", "mglibc", "muclibc", B, G, U)
#else
#error "Unsupported DEFAULT_LIBC"
#endif /* DEFAULT_LIBC */

/* For most targets the following definitions suffice;
   GLIBC_DYNAMIC_LINKER must be defined for each target using them, or
   GLIBC_DYNAMIC_LINKER32 and GLIBC_DYNAMIC_LINKER64 for targets
   supporting both 32-bit and 64-bit compilation.  */
#define UCLIBC_DYNAMIC_LINKER "/lib/ld-uClibc.so.0"
#define UCLIBC_DYNAMIC_LINKER32 "/lib/ld-uClibc.so.0"
#define UCLIBC_DYNAMIC_LINKER64 "/lib/ld64-uClibc.so.0"
#define UCLIBC_DYNAMIC_LINKERX32 "/lib/ldx32-uClibc.so.0"
#define BIONIC_DYNAMIC_LINKER "/system/bin/linker"
#define BIONIC_DYNAMIC_LINKER32 "/system/bin/linker"
#define BIONIC_DYNAMIC_LINKER64 "/system/bin/linker64"
#define BIONIC_DYNAMIC_LINKERX32 "/system/bin/linkerx32"

#define GNU_USER_DYNAMIC_LINKER						\
  CHOOSE_DYNAMIC_LINKER (GLIBC_DYNAMIC_LINKER, UCLIBC_DYNAMIC_LINKER,	\
			 BIONIC_DYNAMIC_LINKER)
#define GNU_USER_DYNAMIC_LINKER32					\
  CHOOSE_DYNAMIC_LINKER (GLIBC_DYNAMIC_LINKER32, UCLIBC_DYNAMIC_LINKER32, \
			 BIONIC_DYNAMIC_LINKER32)
#define GNU_USER_DYNAMIC_LINKER64					\
  CHOOSE_DYNAMIC_LINKER (GLIBC_DYNAMIC_LINKER64, UCLIBC_DYNAMIC_LINKER64, \
			 BIONIC_DYNAMIC_LINKER64)
#define GNU_USER_DYNAMIC_LINKERX32					\
  CHOOSE_DYNAMIC_LINKER (GLIBC_DYNAMIC_LINKERX32, UCLIBC_DYNAMIC_LINKERX32, \
			 BIONIC_DYNAMIC_LINKERX32)

/* Determine whether the entire c99 runtime
   is present in the runtime library.  */
#undef TARGET_C99_FUNCTIONS
#define TARGET_C99_FUNCTIONS (OPTION_GLIBC)

/* Whether we have sincos that follows the GNU extension.  */
#undef TARGET_HAS_SINCOS
#define TARGET_HAS_SINCOS (OPTION_GLIBC || OPTION_BIONIC)
