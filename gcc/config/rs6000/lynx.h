/* Definitions for Rs6000 running LynxOS.
   Copyright (C) 1995 Free Software Foundation, Inc.
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

#include <lynx.h>

/* Definitions we want to override with those from rs6000.h: */
#undef LIB_SPEC
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#undef ASM_FILE_START
#undef EXTRA_SECTIONS
#undef READONLY_DATA_SECTION
#undef EXTRA_SECTION_FUNCTIONS
#undef SELECT_RTX_SECTION
#undef SELECT_SECTION
#undef ASM_OUTPUT_LABELREF
#undef ASM_OUTPUT_INTERNAL_LABEL
#undef ASM_GENERATE_INTERNAL_LABEL
#undef ASM_OUTPUT_COMMON
#undef ASM_OUTPUT_LOCAL
#undef ASM_OUTPUT_CONSTRUCTOR
#undef ASM_OUTPUT_DESTRUCTOR
#undef CTORS_SECTION_FUNCTION
#undef DTORS_SECTION_FUNCTION

#undef SDB_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE

#undef FUNCTION_PROFILER

#include <rs6000/rs6000.h>

/* LynxOS has signed chars, regardless of what most R/S 6000 systems do */
#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 1

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Acpu(rs6000) -Amachine(rs6000) -Asystem(lynx) -Asystem(unix) -DLynx -D_IBMR2 -Dunix -Drs6000 -Dlynx -DLYNX"

#undef LINK_SPEC
#define LINK_SPEC "-T0x10001000 -H0x1000 -D0x20000000 -btextro -bhalt:4 -bnodelcsect -bnso -bro -bnoglink %{v} %{b*}"

#undef LIB_SPEC
#define LIB_SPEC "%{mthreads:-L/lib/thread/}  \
  %{msystem-v:-lc_v -lm.v}  \
  %{!msystem-v:%{mposix:-lc_p} -lc -lm}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{p:%{mthreads:thread/pinit.o%s}%{!mthreads:pinit.o%s}}%{!p:%{msystem-v:vinit.o%s -e_start}%{!msystem-v:%{mthreads:thread/init.o%s}%{!mthreads:init.o%s}}}"

#undef ENDFILE_SPEC

/* This can become more refined as we have more powerpc options. */
#undef ASM_SPEC
#define ASM_SPEC "-u %{m601:-m601} %{power*:-m601}"

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
#undef REAL_NM_FILE_NAME
#undef REAL_STRIP_FILE_NAME

/* LynxOS doesn't have mcount. */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(file, profile_label_no)
