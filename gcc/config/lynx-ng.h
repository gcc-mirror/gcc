/* Target independent definitions for LynxOS, using Lynx's old as and ld.
   Copyright (C) 1993, 1999 Free Software Foundation, Inc.

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

/* This is for backwards compatibility with older Lynx tools, which use
   a version of a.out format. */

#undef ASM_SPEC
#define ASM_SPEC "%{mcoff:-C}"

#undef CPP_SPEC
#define CPP_SPEC "%{mthreads:-D_MULTITHREADED}  \
  %{mposix:-D_POSIX_SOURCE}  \
  %{msystem-v:-I/usr/include_v}"

/* Provide required defaults for linker switches.  */

#undef LINK_SPEC
#define LINK_SPEC "%{msystem-v:-V} %{mcoff:-k}"

#undef LIB_SPEC
#define LIB_SPEC "%{mthreads:-L/lib/thread/}%{msystem-v:-lc_v}%{!msystem-v:%{mposix:-lc_p} -lc}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{p:%{mcoff:pinit1.o%s}%{!mcoff:pinit.o%s}}%{!p:%{msystem-v:%{mcoff:vinit1.o%s}%{!mcoff:vinit.o%s}}%{!msystem-v:%{mcoff:init1.o%s}%{!mcoff:init.o%s}}}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "%{mcoff:initn.o%s} %{p:_etext.o%s}"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

/* We want to output DBX debugging information.  */

#define DBX_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* We optionally want to be able to produce SDB debugging output so that
   we can create debuggable SDB/coff files.  This won't be needed when
   stabs-in-coff works.  */

#define SDB_DEBUGGING_INFO

/* Generate calls to memcpy, memcmp and memset.  */

#define TARGET_MEM_FUNCTIONS

/* Handle #pragma pack and sometimes #pragma weak.  */

#define HANDLE_SYSV_PRAGMA

#define TARGET_THREADS	(target_flags & MASK_THREADS)
#define MASK_THREADS	0x40000000

#define TARGET_POSIX	(target_flags & MASK_POSIX)
#define MASK_POSIX	0x20000000

#define TARGET_SYSTEM_V	(target_flags & MASK_SYSTEM_V)
#define MASK_SYSTEM_V	0x10000000

#define TARGET_COFF	(target_flags & MASK_COFF)
#define MASK_COFF	0x08000000

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
    {"threads",		MASK_THREADS},		\
    {"posix",		MASK_POSIX},		\
    {"system-v",	MASK_SYSTEM_V},		\
    {"coff",		MASK_COFF},

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS \
{ if (TARGET_SYSTEM_V && profile_flag)			\
    warning ("-msystem-v and -p are incompatible");		\
  if (TARGET_SYSTEM_V && TARGET_THREADS)			\
    warning ("-msystem-v and -mthreads are incompatible"); }

/* This is defined only so that we can find the assembler.  Everything else
   is in /bin.  */

#define MD_EXEC_PREFIX "/usr/local/lib/gcc-"

/* This is needed because /bin/ld does not handle -L options correctly. */

#define LINK_LIBGCC_SPECIAL_1

/* The Lynx linker considers __main to be a possible entry point, so we
   must use a different name.  */

#define NAME__MAIN "____main"
#define SYMBOL__MAIN ____main
