/* Definitions for TI C6X running ucLinux using ELF
   Copyright (C) 2010-2014 Free Software Foundation, Inc.
   Contributed by Andrew Jenner <andrew@codesourcery.com>
   Contributed by Bernd Schmidt <bernds@codesourcery.com>

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

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() 		\
  do 						\
    {						\
      builtin_define ("__uClinux__");		\
      builtin_define_std ("linux");             \
      builtin_define_std ("unix");              \
      builtin_assert ("system=linux");          \
      builtin_assert ("system=unix");           \
      builtin_assert ("system=posix");          \
    }						\
  while (false)

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared:crt1%O%s} crti%O%s %{shared|pie:crtbeginS.o%s;:crtbegin.o%s}"

#define UCLIBC_DYNAMIC_LINKER "/lib/ld-uClibc.so.0"

#undef LINK_SPEC
#define LINK_SPEC ENDIAN_LINK_SPEC \
  "%{shared} %{fpie|fPIE:-pie} \
  %{!shared: %{!static: \
   %{rdynamic:-export-dynamic} \
   %{!dynamic-linker:-dynamic-linker " UCLIBC_DYNAMIC_LINKER "}} \
   %{static}}"

#undef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS "%{!mno-dsbt:-mdsbt}"

/* Clear the instruction cache from `beg' to `end'.  This makes an
   inline system call to SYS_cacheflush.  */
#undef CLEAR_INSN_CACHE
#define CLEAR_INSN_CACHE(BEG, END)					\
{									\
  register unsigned long _beg __asm ("A4") = (unsigned long) (BEG);	\
  register unsigned long _end __asm ("B4") = (unsigned long) (END);	\
  register unsigned long _scno __asm ("B0") = 244;			\
  __asm __volatile ("swe		; sys_cache_sync"		\
		    : "=a" (_beg)					\
		    : "0" (_beg), "b" (_end), "b" (_scno));	\
}
