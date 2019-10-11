/* Definitions for x86-64 for Zephyr 64-bit toolchain.
   Copyright (C) 2001-2019 Free Software Foundation, Inc.
   Contributed by Jan Hubicka <jh@suse.cz>, based on linux.h.
   Based on gnu-user64.h.

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

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crtbegin.o%s"

#undef	LINK_SPEC
#define LINK_SPEC \
	"%{m32:-m elf_i386} \
	 %{m64:-m elf_x86_64} \
	 %{mx32:-m elf32_x86_64}"

#define MULTILIB_DEFAULTS { "m64" }
