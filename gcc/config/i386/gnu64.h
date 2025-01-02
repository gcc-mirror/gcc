/* Configuration for an x86_64 running GNU with ELF as the target machine.  */

/*
Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

#define GNU_USER_LINK_EMULATION32 "elf_i386"
#define GNU_USER_LINK_EMULATION64 "elf_x86_64"
#define GNU_USER_LINK_EMULATIONX32 "elf32_x86_64"

#undef GNU_USER_DYNAMIC_LINKER
#define GNU_USER_DYNAMIC_LINKER32 "/lib/ld.so.1"
#define GNU_USER_DYNAMIC_LINKER64 "/lib/ld-x86-64.so.1"
#define GNU_USER_DYNAMIC_LINKERX32 "/lib/ld-x32.so.1"

#undef	STARTFILE_SPEC
#if defined HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:%{static-pie:grcrt0.o%s;static:gcrt0.o%s;:gcrt1.o%s};static-pie:rcrt0.o%s;static:crt0.o%s;" PIE_SPEC ":Scrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|static-pie|" PIE_SPEC ":crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:%{static:gcrt0.o%s;:gcrt1.o%s};static:crt0.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|static-pie|" PIE_SPEC ":crtbeginS.o%s;:crtbegin.o%s}"
#endif
