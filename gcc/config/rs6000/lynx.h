/* Definitions for Rs6000 running LynxOS.
   Copyright (C) 1995, 1996, 2000, 2002 Free Software Foundation, Inc.
   Contributed by David Henkel-Wallace, Cygnus Support (gumby@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Definitions we want to override with those from rs6000.h: */
#undef LIB_SPEC
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#undef ASM_FILE_START
#undef EXTRA_SECTIONS
#undef READONLY_DATA_SECTION
#undef READONLY_DATA_SECTION_ASM_OP
#undef EXTRA_SECTION_FUNCTIONS
#undef TARGET_ASM_SELECT_RTX_SECTION
#undef TARGET_ASM_SELECT_SECTION
#undef USER_LABEL_PREFIX
#undef ASM_OUTPUT_LABELREF
#undef ASM_OUTPUT_INTERNAL_LABEL
#undef ASM_GENERATE_INTERNAL_LABEL
#undef ASM_OUTPUT_COMMON
#undef ASM_OUTPUT_LOCAL

#undef SDB_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE

#undef FUNCTION_PROFILER

#include <rs6000/rs6000.h>

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (LynxOS-RS/6000)");

/* LynxOS has signed chars, regardless of what most R/S 6000 systems do */
#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 1

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()         \
  do                                     \
    {                                    \
      builtin_assert ("cpu=rs6000");     \
      builtin_assert ("machine=rs6000"); \
      builtin_assert ("system=lynx");    \
      builtin_assert ("system=unix");    \
      builtin_define_std ("Lynx");       \
      builtin_define ("_IBMR2");         \
      builtin_define_std ("unix");       \
      builtin_define_std ("rs6000");     \
      builtin_define_std ("lynx");       \
      builtin_define_std ("LYNX");       \
    }                                    \
  while (0)

#undef LINK_SPEC
#define LINK_SPEC "-T0x10001000 -H0x1000 -D0x20000000 -btextro -bhalt:4 -bnodelcsect -bnso -bro -bnoglink %{v} %{b*}"

#undef LIB_SPEC
#define LIB_SPEC "%{mthreads:-L/lib/thread/}  \
  %{msystem-v:-lc_v -lm.v}  \
  %{!msystem-v:%{mposix:-lc_p} -lc -lm}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{p:%{mthreads:thread/pinit.o%s}%{!mthreads:pinit.o%s}}%{!p:%{msystem-v:vinit.o%s -e_start}%{!msystem-v:%{mthreads:thread/init.o%s}%{!mthreads:init.o%s}}}"

#undef ENDFILE_SPEC

/* This can become more refined as we have more powerpc options.  */
#undef ASM_SPEC
#define ASM_SPEC "-u %(asm_cpu)"

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
    {"threads",		MASK_THREADS},		\
    {"posix",		MASK_POSIX},		\
    {"system-v",	MASK_SYSTEM_V},

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS \
do {								\
  if (TARGET_SYSTEM_V && profile_flag)				\
    warning ("-msystem-v and -p are incompatible");		\
  if (TARGET_SYSTEM_V && TARGET_THREADS)			\
    warning ("-msystem-v and -mthreads are incompatible");	\
} while (0)

/* For collect2 */
#define OBJECT_FORMAT_NONE
#undef OBJECT_FORMAT_COFF
#undef OBJECT_FORMAT_ROSE
#undef MD_EXEC_PREFIX
#undef REAL_LD_FILE_NAME
#undef REAL_STRIP_FILE_NAME

/* LynxOS doesn't have mcount.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(file, profile_label_no)
