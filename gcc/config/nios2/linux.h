/* Definitions of target support for Altera Nios II systems
   running GNU/Linux with ELF format.
   Copyright (C) 2012-2016 Free Software Foundation, Inc.
   Contributed by Mentor Graphics, Inc.

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

#define TARGET_OS_CPP_BUILTINS()                \
  do                                            \
    {                                           \
      GNU_USER_TARGET_OS_CPP_BUILTINS();           \
    }                                           \
  while (0)

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux-nios2.so.1"

#undef LINK_SPEC
#define LINK_SPEC LINK_SPEC_ENDIAN \
 "%{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker " GNU_USER_DYNAMIC_LINKER "} \
    %{static:-static}}"

/* This toolchain implements the ABI for Linux Systems documented in the
   Nios II Processor Reference Handbook.  */
#define TARGET_LINUX_ABI 1

