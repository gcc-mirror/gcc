/* Linux Definitions for OpenRISC.
   Copyright (C) 2018-2021 Free Software Foundation, Inc.
   Contributed by Stafford Horne.

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

#ifndef GCC_OR1K_LINUX_H
#define GCC_OR1K_LINUX_H

/* elfos.h should have already been included.  Now just override
   any conflicting definitions and add any extras.  */

#define TARGET_OS_CPP_BUILTINS() \
  GNU_USER_TARGET_OS_CPP_BUILTINS ()

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux-or1k.so.1"

#undef MUSL_DYNAMIC_LINKER
#define MUSL_DYNAMIC_LINKER  "/lib/ld-musl-or1k.so.1"

#undef LINK_SPEC
#define LINK_SPEC "%{h*}			\
   %{static:-Bstatic}				\
   %{shared:-shared}				\
   %{symbolic:-Bsymbolic}			\
   %{!static:%{!static-pie:			\
     %{rdynamic:-export-dynamic}		\
     %{!shared:-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}}} \
   %{static-pie:-Bstatic -pie --no-dynamic-linker -z text}"

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

#endif /* GCC_OR1K_LINUX_H */
