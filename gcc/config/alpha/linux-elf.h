/* Definitions of target machine for GNU compiler
   for Alpha Linux-based GNU systems using ELF.
   Copyright (C) 1996-2014 Free Software Foundation, Inc.
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

#undef EXTRA_SPECS
#define EXTRA_SPECS \
{ "elf_dynamic_linker", ELF_DYNAMIC_LINKER },

#define GLIBC_DYNAMIC_LINKER	"/lib/ld-linux.so.2"
#define UCLIBC_DYNAMIC_LINKER "/lib/ld-uClibc.so.0"
#if DEFAULT_LIBC == LIBC_UCLIBC
#define CHOOSE_DYNAMIC_LINKER(G, U) "%{mglibc:" G ";:" U "}"
#elif DEFAULT_LIBC == LIBC_GLIBC
#define CHOOSE_DYNAMIC_LINKER(G, U) "%{muclibc:" U ";:" G "}"
#else
#error "Unsupported DEFAULT_LIBC"
#endif
#define GNU_USER_DYNAMIC_LINKER \
  CHOOSE_DYNAMIC_LINKER (GLIBC_DYNAMIC_LINKER, UCLIBC_DYNAMIC_LINKER)

#define ELF_DYNAMIC_LINKER	GNU_USER_DYNAMIC_LINKER

#define LINK_SPEC "-m elf64alpha %{G*} %{relax:-relax}		\
  %{O*:-O3} %{!O*:-O1}						\
  %{shared:-shared}						\
  %{!shared:							\
    %{!static:							\
      %{rdynamic:-export-dynamic}				\
      -dynamic-linker %(elf_dynamic_linker)}	\
    %{static:-static}}"

#undef LIB_SPEC
#define LIB_SPEC \
"%{pthread:-lpthread} %{shared:-lc}%{!shared:%{profile:-lc_p}%{!profile:-lc}} "

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack
