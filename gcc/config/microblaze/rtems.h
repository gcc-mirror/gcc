/* Definitions for rtems targeting a microblaze using ELF.
   Copyright (C) 2012-2016 Free Software Foundation, Inc.

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

/* Specify predefined symbols in preprocessor.  */

#define TARGET_OS_CPP_BUILTINS() do {		\
  builtin_define( "__rtems__" );		\
  builtin_assert( "system=rtems" );		\
} while (0)

/* Redefine to include only items relevant for RTEMS */
#undef LINK_SPEC
#define LINK_SPEC "%{shared:-shared} -N -relax \
  %{mbig-endian:-EB --oformat=elf32-microblaze} \
  %{mlittle-endian:-EL --oformat=elf32-microblazeel} \
  %{mxl-gp-opt:%{G*}} %{!mxl-gp-opt: -G 0}"
