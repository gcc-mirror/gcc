/* Definitions for TILE-Gx running Linux-based GNU systems with ELF.
   Copyright (C) 2011-2020 Free Software Foundation, Inc.
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

#undef ASM_SPEC
#define ASM_SPEC "%(endian_spec) %{m32:--32} %{m64:--64}"

#undef	LINK_SPEC
#define LINK_SPEC "%(endian_spec) \
  %{m64:-m elf64tilegx} %{m32:-m elf32tilegx} \
  %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker \
        %{ m32: /lib32/ld.so.1} \
        %{!m32: /lib/ld.so.1}} \
    %{static:-static}}"

#define MULTILIB_DEFAULTS { "m64" }

#define NO_PROFILE_COUNTERS	1

#undef MCOUNT_NAME
#define MCOUNT_NAME "__mcount"

#undef NEED_INDICATE_EXEC_STACK
#define NEED_INDICATE_EXEC_STACK 1

#ifdef TARGET_LIBC_PROVIDES_SSP
/* TILE-Gx glibc provides __stack_chk_guard two pointer-size words before
   tp. */
#define TARGET_THREAD_SSP_OFFSET (-2 * GET_MODE_SIZE (ptr_mode))
#endif

/* For __clear_cache in libgcc2.c.  */
#ifdef IN_LIBGCC2

/* Use the minimum page size of 4K.  Alternatively we can call
   getpagesize() but it introduces a libc dependence.
   See Linux arch/tile/include/uapi/arch/icache.h for more commentary.  */
#undef CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE(BEG, END)                                      \
{                                                                       \
  long size = (long) (END) - (long) (BEG);                              \
  if (size)                                                             \
    {                                                                   \
      const char *p = (const char *) ((unsigned long) (BEG) & -64L);    \
      const char *end = p + (size < 4096 ? size : 4096) - 1;            \
      __insn_mf ();                                                     \
      for (; p <= end; p += 64)                                         \
        __insn_icoh (p);                                                \
      __insn_drain ();                                                  \
    }                                                                   \
}

#else

/* define CLEAR_INSN_CACHE so that gcc knows to expand __builtin__clear_cache
   to the libraray call.  */
#undef CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE 1

#endif
