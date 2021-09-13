/* Xtensa uClinux configuration.
   Derived from the configuration for GCC for Intel i386 running Linux.
   Copyright (C) 2001-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()				\
  do								\
    {								\
      GNU_USER_TARGET_OS_CPP_BUILTINS ();			\
      builtin_define ("__uClinux__");				\
    }								\
  while (0)

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef ASM_SPEC
#define ASM_SPEC \
 "%{mtext-section-literals:--text-section-literals} \
  %{mno-text-section-literals:--no-text-section-literals} \
  %{mtarget-align:--target-align} \
  %{mno-target-align:--no-target-align} \
  %{mlongcalls:--longcalls} \
  %{mno-longcalls:--no-longcalls} \
  %{mauto-litpools:--auto-litpools} \
  %{mno-auto-litpools:--no-auto-litpools} \
  %{mabi=windowed:--abi-windowed} \
  %{mabi=call0:--abi-call0}"

#undef LINK_SPEC
#define LINK_SPEC \
 "%{!no-elf2flt:%{!elf2flt*:-elf2flt}} \
  %{mabi=windowed:--abi-windowed} \
  %{mabi=call0:--abi-call0}"

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX	"."

/* Don't enable "-fpic" for Xtensa uclinux.  */
#define XTENSA_ALWAYS_PIC 0

#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION no_c99_libc_has_function

#undef DBX_REGISTER_NUMBER

