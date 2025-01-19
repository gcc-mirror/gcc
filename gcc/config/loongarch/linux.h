/* Definitions for Linux-based systems with libraries in ELF format.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

/* Default system library search paths.
 * This ensures that a compiler configured with --disable-multilib
 * can work in a multilib environment.  */

#if !defined(LA_DEFAULT_TARGET_MUSL) \
  && defined(LA_DISABLE_MULTILIB) \
  && defined(LA_DISABLE_MULTIARCH)

  #if DEFAULT_ABI_BASE == ABI_BASE_LP64D
    #define ABI_LIBDIR "lib64"
  #elif DEFAULT_ABI_BASE == ABI_BASE_LP64F
    #define ABI_LIBDIR "lib64/f32"
  #elif DEFAULT_ABI_BASE == ABI_BASE_LP64S
    #define ABI_LIBDIR "lib64/sf"
  #endif

#endif

#ifndef ABI_LIBDIR
#define ABI_LIBDIR "lib"
#endif

#define STANDARD_STARTFILE_PREFIX_1 "/" ABI_LIBDIR "/"
#define STANDARD_STARTFILE_PREFIX_2 "/usr/" ABI_LIBDIR "/"


/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* The default value isn't sufficient in 64-bit mode.  */
#define STACK_CHECK_PROTECT (TARGET_64BIT ? 16 * 1024 : 12 * 1024)

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

/* The stack pointer needs to be moved while checking the stack.  */
#define STACK_CHECK_MOVING_SP 1
