/* Configuration for an i386 running GNU with ELF as the target machine.  */

/*
Copyright (C) 1994-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC.  If not, see <http://www.gnu.org/licenses/>.
*/

#define GNU_USER_LINK_EMULATION "elf_i386"

#undef GNU_USER_DYNAMIC_LINKER
#define GNU_USER_DYNAMIC_LINKER "/lib/ld.so"

#undef	STARTFILE_SPEC
#if defined HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt0.o%s;pie:Scrt1.o%s;static:crt0.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt0.o%s;static:crt0.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#endif

#ifdef TARGET_LIBC_PROVIDES_SSP

/* Not supported yet.  */
# undef TARGET_THREAD_SSP_OFFSET

/* Not supported yet.  */
# undef TARGET_CAN_SPLIT_STACK
# undef TARGET_THREAD_SPLIT_STACK_OFFSET

#endif
