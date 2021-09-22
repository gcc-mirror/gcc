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

/* Default system library search paths.
 * This ensures that a compiler configured with --disable-multilib
 * can work in an multilib environment.  */

#if defined(__DISABLE_MULTILIB) && defined(__DISABLE_MULTIARCH)

  /* Integer ABI */
  #if DEFAULT_ABI_INT == ABI_LP64
    #define INT_ABI_SUFFIX "lib64"
  #endif

  /* Floating-Point ABI */
  #if DEFAULT_ABI_FLOAT == ABI_SOFT_FLOAT
    #define FLOAT_ABI_SUFFIX "soft/"
  #elif DEFAULT_ABI_FLOAT == ABI_SINGLE_FLOAT
    #define FLOAT_ABI_SUFFIX "single/"
  #endif

#endif

#ifndef INT_ABI_SUFFIX
#define INT_ABI_SUFFIX "lib"
#endif

#ifndef FLOAT_ABI_SUFFIX
#define FLOAT_ABI_SUFFIX ""
#endif

#define STANDARD_STARTFILE_PREFIX_1 "/" INT_ABI_SUFFIX "/" FLOAT_ABI_SUFFIX
#define STANDARD_STARTFILE_PREFIX_2 "/usr/" INT_ABI_SUFFIX "/" FLOAT_ABI_SUFFIX


/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* FIXME*/
/* The default value isn't sufficient in 64-bit mode.  */
#define STACK_CHECK_PROTECT (TARGET_64BIT ? 16 * 1024 : 12 * 1024)
