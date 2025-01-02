/* Definitions of target machine for GNU compiler.  Vxworks Aarch 64bit
   version.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
   Contributed by Douglas B Rupp

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

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS VXWORKS_OVERRIDE_OPTIONS

#undef LINK_SPEC
#define LINK_SPEC VXWORKS_LINK_SPEC

#undef LIB_SPEC
#define LIB_SPEC VXWORKS_LIB_SPEC

#undef STARTFILE_SPEC
#define STARTFILE_SPEC VXWORKS_STARTFILE_SPEC

#undef ENDFILE_SPEC
#define ENDFILE_SPEC VXWORKS_ENDFILE_SPEC

#undef CPP_SPEC
#define CPP_SPEC VXWORKS_ADDITIONAL_CPP_SPEC

#undef CC1_SPEC
#define CC1_SPEC VXWORKS_CC1_SPEC

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER VXWORKS_FUNCTION_PROFILER

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()	  \
  do {					  \
    if (TARGET_BIG_END)                         \
      builtin_define ("ARMEB");                 \
    else                                        \
      builtin_define ("ARMEL");                 \
    builtin_define \
      (VX_CPU_PREFIX "CPU=" VX_CPU_PREFIX "ARMARCH8A");	\
    VXWORKS_OS_CPP_BUILTINS ();		  \
  } while (0)

/* Static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#undef STACK_CHECK_PROTECT
#define STACK_CHECK_PROTECT 16384

/* The VxWorks environment on aarch64 is llvm-based.  */
#undef VXWORKS_PERSONALITY
#define VXWORKS_PERSONALITY "llvm"

/* VxWorks uses R18 as a TCB pointer.  We must pick something else as
   the static chain and R18 needs to be claimed "fixed".  Until we
   arrange to override the common parts of the port family to
   acknowledge the latter, configure --with-specs="-ffixed-r18".  */
#undef  STATIC_CHAIN_REGNUM
#define STATIC_CHAIN_REGNUM 9

