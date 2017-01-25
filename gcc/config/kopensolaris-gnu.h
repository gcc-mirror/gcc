/* Definitions for kOpenSolaris-based GNU systems with ELF format
   Copyright (C) 2004-2017 Free Software Foundation, Inc.
   Contributed by Robert Millan.

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

#undef GNU_USER_TARGET_OS_CPP_BUILTINS    
#define GNU_USER_TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define ("__OpenSolaris_kernel__");	\
	builtin_define ("__GLIBC__");		\
	builtin_define_std ("unix");		\
	builtin_assert ("system=unix");		\
	builtin_assert ("system=posix");	\
    }						\
  while (0)

#undef GNU_USER_DYNAMIC_LINKER
#define GNU_USER_DYNAMIC_LINKER "/lib/ld.so.1"
