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

#define GLIBC_DYNAMIC_LINKER \
  "%{mnan=2008:/lib/ld-linux-mipsn8.so.1;:/lib/ld.so.1}"

#undef UCLIBC_DYNAMIC_LINKER
#define UCLIBC_DYNAMIC_LINKER \
  "%{mnan=2008:/lib/ld-uClibc-mipsn8.so.0;:/lib/ld-uClibc.so.0}"
