/* Definitions for rtems targeting a NIOS2 using ELF.
   Copyright (C) 2011-2017 Free Software Foundation, Inc.

   Contributed by Chris Johns (chrisj@rtems.org).

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
#define TARGET_OS_CPP_BUILTINS()        \
do {                                    \
  builtin_define ("__rtems__");         \
  builtin_define ("__USE_INIT_FINI__"); \
  builtin_assert ("system=rtems");      \
} while (0)

/* This toolchain implements the ABI for Linux Systems documented in the
   Nios II Processor Reference Handbook.

   This is done so RTEMS targets have Thread Local Storage like Linux.  */
#define TARGET_LINUX_ABI 1
