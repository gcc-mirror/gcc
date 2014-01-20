/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_LINUX_H
#define GCC_AARCH64_LINUX_H

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux-aarch64%{mbig-endian:_be}.so.1"

#define CPP_SPEC "%{pthread:-D_REENTRANT}"

#define LINUX_TARGET_LINK_SPEC  "%{h*}		\
   %{static:-Bstatic}				\
   %{shared:-shared}				\
   %{symbolic:-Bsymbolic}			\
   %{rdynamic:-export-dynamic}			\
   -dynamic-linker " GNU_USER_DYNAMIC_LINKER "	\
   -X						\
   %{mbig-endian:-EB} %{mlittle-endian:-EL}     \
   -maarch64linux%{mbig-endian:b}"

#define LINK_SPEC LINUX_TARGET_LINK_SPEC

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	GNU_USER_TARGET_OS_CPP_BUILTINS();	\
    }						\
  while (0)

#endif  /* GCC_AARCH64_LINUX_H */
