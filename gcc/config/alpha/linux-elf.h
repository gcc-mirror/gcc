/* Definitions of target machine for GNU compiler
   for Alpha Linux-based GNU systems using ELF.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Richard Henderson.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (Alpha GNU/Linux for ELF)");

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
{ "elf_dynamic_linker", ELF_DYNAMIC_LINKER },

#undef SUB_CPP_PREDEFINES
#define SUB_CPP_PREDEFINES	"-D__ELF__"

#ifdef USE_GNULIBC_1
#define ELF_DYNAMIC_LINKER	"/lib/ld.so.1"
#else
#define ELF_DYNAMIC_LINKER	"/lib/ld-linux.so.2"
#endif

#ifndef USE_GNULIBC_1
#undef DEFAULT_VTABLE_THUNKS
#define DEFAULT_VTABLE_THUNKS 1
#endif

#ifndef USE_GNULIBC_1
#undef LIB_SPEC
#define LIB_SPEC \
"%{shared:-lc}%{!shared:%{pthread:-lpthread }%{profile:-lc_p}%{!profile:-lc}} "
#endif
