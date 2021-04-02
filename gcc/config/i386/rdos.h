/* Definitions for RDOS on i386.
   Copyright (C) 2013-2021 Free Software Foundation, Inc.

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

/* RDOS uses .exe suffix */
#undef TARGET_EXECUTABLE_SUFFIX
#define TARGET_EXECUTABLE_SUFFIX ".exe"

#undef TARGET_TLS_DIRECT_SEG_REFS_DEFAULT
#define TARGET_TLS_DIRECT_SEG_REFS_DEFAULT MASK_TLS_DIRECT_SEG_REFS

#undef DEFAULT_TLS_SEG_REG
#define DEFAULT_TLS_SEG_REG ADDR_SPACE_SEG_GS

#undef TARGET_RDOS
#define TARGET_RDOS 1

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__RDOS__");		\
      builtin_assert ("system=rdos");		\
    }						\
  while (0)

#define TARGET_RUST_OS_INFO()		\
  do {						\
    /*TODO: not supported by rustc and so subject to change - based on triple found online*/ \
    /*this seems to not refer to the 70s Data General RDOS, but one partly compatible with win32*/ \
    /*as such, target_family could be windows*/ \
    builtin_rust_info ("target_family", "");	\
    builtin_rust_info ("target_os", "rdos");	\
    builtin_rust_info ("target_vendor", "pc");	\
    builtin_rust_info ("target_env", "");	\
  } while (0)
