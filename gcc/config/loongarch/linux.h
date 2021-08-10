/* Definitions for LoongArch running Linux-based GNU systems with ELF format.
   Copyright (C) 1998-2018 Free Software Foundation, Inc.

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

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() \
  do \
    { \
      GNU_USER_TARGET_OS_CPP_BUILTINS (); \
      /* The GNU C++ standard library requires this.  */ \
      if (c_dialect_cxx ()) \
	builtin_define ("_GNU_SOURCE"); \
    } \
  while (0)

#define GNU_USER_LINK_EMULATION32 "elf32loongarch"
#define GNU_USER_LINK_EMULATION64 "elf64loongarch"

#define GLIBC_DYNAMIC_LINKERLP32 "/lib32/ld.so.1"
#define GLIBC_DYNAMIC_LINKERLP64 "/lib64/ld.so.1"

#define GNU_USER_DYNAMIC_LINKERLP32 GLIBC_DYNAMIC_LINKERLP32
#define GNU_USER_DYNAMIC_LINKERLP64 GLIBC_DYNAMIC_LINKERLP64

#undef LINK_SPEC
#define LINK_SPEC GNU_USER_TARGET_LINK_SPEC

#undef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC GNU_USER_TARGET_CC1_SPEC

#undef LIB_SPEC
#define LIB_SPEC GNU_USER_TARGET_LIB_SPEC

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* FIXME*/
/* The default value isn't sufficient in 64-bit mode.  */
#define STACK_CHECK_PROTECT (TARGET_64BIT ? 16 * 1024 : 12 * 1024)
