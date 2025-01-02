/* Definitions of target machine for GNU compiler,
   for SPARC targeting the VxWorks run time environment.
   Copyright (C) 2007-2025 Free Software Foundation, Inc.

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

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__sparc");		\
      builtin_define ("CPU=SIMSPARCSOLARIS");	\
      VXWORKS_OS_CPP_BUILTINS ();		\
    }						\
  while (0)

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS VXWORKS_OVERRIDE_OPTIONS

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC VXWORKS_ADDITIONAL_CPP_SPEC

#undef ASM_SPEC
#define ASM_SPEC "%{" FPIE_OR_FPIC_SPEC ":-K PIC} %(asm_cpu)"

#undef LIB_SPEC
#define LIB_SPEC VXWORKS_LIB_SPEC
#undef LINK_SPEC
#define LINK_SPEC VXWORKS_LINK_SPEC
#undef STARTFILE_SPEC
#define STARTFILE_SPEC VXWORKS_STARTFILE_SPEC
#undef ENDFILE_SPEC
#define ENDFILE_SPEC VXWORKS_ENDFILE_SPEC

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER VXWORKS_FUNCTION_PROFILER

/* Use standard numbered ctors/dtors sections.  */
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

/* We cannot use PC-relative accesses for VxWorks PIC because there is no
   fixed gap between segments.  */
#undef ASM_PREFERRED_EH_DATA_FORMAT

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* This platform supports the probing method of stack checking (RTP mode).
   8K is reserved in the stack to propagate exceptions in case of overflow.  */
#define STACK_CHECK_PROTECT 8192

/* SPARC_LONG_DOUBLE_TYPE_SIZE should be defined per OS.  */
#undef SPARC_LONG_DOUBLE_TYPE_SIZE
#define SPARC_LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
