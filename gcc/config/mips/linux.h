/* Definitions for MIPS running Linux-based GNU systems with ELF format.
   Copyright (C) 1998-2014 Free Software Foundation, Inc.

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

#define GNU_USER_LINK_EMULATION32 "elf32%{EB:b}%{EL:l}tsmip"
#define GNU_USER_LINK_EMULATION64 "elf64%{EB:b}%{EL:l}tsmip"
#define GNU_USER_LINK_EMULATIONN32 "elf32%{EB:b}%{EL:l}tsmipn32"

#define GLIBC_DYNAMIC_LINKER32 \
  "%{mnan=2008:/lib/ld-linux-mipsn8.so.1;:/lib/ld.so.1}"
#define GLIBC_DYNAMIC_LINKER64 \
  "%{mnan=2008:/lib64/ld-linux-mipsn8.so.1;:/lib64/ld.so.1}"
#define GLIBC_DYNAMIC_LINKERN32 \
  "%{mnan=2008:/lib32/ld-linux-mipsn8.so.1;:/lib32/ld.so.1}"

#undef UCLIBC_DYNAMIC_LINKER32
#define UCLIBC_DYNAMIC_LINKER32 \
  "%{mnan=2008:/lib/ld-uClibc-mipsn8.so.0;:/lib/ld-uClibc.so.0}"
#undef UCLIBC_DYNAMIC_LINKER64
#define UCLIBC_DYNAMIC_LINKER64 \
  "%{mnan=2008:/lib/ld64-uClibc-mipsn8.so.0;:/lib/ld64-uClibc.so.0}"
#define UCLIBC_DYNAMIC_LINKERN32 \
  "%{mnan=2008:/lib32/ld-uClibc-mipsn8.so.0;:/lib32/ld-uClibc.so.0}"

#define BIONIC_DYNAMIC_LINKERN32 "/system/bin/linker32"
#define GNU_USER_DYNAMIC_LINKERN32 \
  CHOOSE_DYNAMIC_LINKER (GLIBC_DYNAMIC_LINKERN32, UCLIBC_DYNAMIC_LINKERN32, \
                         BIONIC_DYNAMIC_LINKERN32)
