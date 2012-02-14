/* Definitions for TILEPro running Linux-based GNU systems with ELF.
   Copyright (C) 2011, 2012
   Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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

#undef CPP_SPEC
#define CPP_SPEC "%{pthread:-D_REENTRANT}"

#undef	LINK_SPEC
#define LINK_SPEC "\
  %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker /lib/ld.so.1} \
    %{static:-static}}"

#define NO_PROFILE_COUNTERS	1

#undef MCOUNT_NAME
#define MCOUNT_NAME "__mcount"

#undef NEED_INDICATE_EXEC_STACK
#define NEED_INDICATE_EXEC_STACK 1

#ifdef TARGET_LIBC_PROVIDES_SSP
/* TILEPro glibc provides __stack_chk_guard two pointer-size words before
   tp. */
#define TARGET_THREAD_SSP_OFFSET (-2 * GET_MODE_SIZE (ptr_mode))
#endif

/* For __clear_cache in libgcc2.c.  */
#ifdef IN_LIBGCC2

#include <arch/icache.h>

/* Use the minimum page size of 4K.  Alternatively we can call getpagesize()
   but it introduces a libc dependence.  */
#undef CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE(beg, end) invalidate_icache (beg, end - beg, 4096)

#else

/* define CLEAR_INSN_CACHE so that gcc knows to expand __builtin__clear_cache
   to the libraray call.  */
#undef CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE 1

#endif
