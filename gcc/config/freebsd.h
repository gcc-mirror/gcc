/* Base configuration file for all FreeBSD targets.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* Common FreeBSD configuration. 
   All FreeBSD architectures should include this file, which will specify
   their commonalities.
   Adapted from gcc/config/i386/freebsd-elf.h by 
   David O'Brien <obrien@FreeBSD.org>.  */


/* This defines which switch letters take arguments.  On FreeBSD, most of
   the normal cases (defined in gcc.c) apply, and we also have -h* and
   -z* options (for the linker) (coming from SVR4).
   We also have -R (alias --rpath), no -z, --soname (-h), --assert etc.  */

#define FBSD_SWITCH_TAKES_ARG(CHAR)					\
  (DEFAULT_SWITCH_TAKES_ARG (CHAR)					\
    || (CHAR) == 'h'							\
    || (CHAR) == 'z' /* ignored by ld */				\
    || (CHAR) == 'R')

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) (FBSD_SWITCH_TAKES_ARG(CHAR))

#define FBSD_WORD_SWITCH_TAKES_ARG(STR)					\
  (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)					\
   || !strcmp ((STR), "rpath") || !strcmp ((STR), "rpath-link")		\
   || !strcmp ((STR), "soname") || !strcmp ((STR), "defsym") 		\
   || !strcmp ((STR), "assert") || !strcmp ((STR), "dynamic-linker"))

#undef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR) (FBSD_WORD_SWITCH_TAKES_ARG(STR))

/* Provide a CPP_SPEC appropriate for FreeBSD.  We just deal with the GCC 
   option `-posix', and PIC issues.  */

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu)						\
  %{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__}		\
  %{posix:-D_POSIX_SOURCE}"

/* Provide a LIB_SPEC appropriate for FreeBSD.  Just select the appropriate
   libc, depending on whether we're doing profiling or need threads support.
   (simular to the default, except no -lg, and no -p).  */

#undef LIB_SPEC
#define LIB_SPEC							\
  "%{!shared:								\
     %{!pg:%{!pthread:%{!kthread:-lc}					\
       %{kthread:-lpthread -lc}}					\
       %{pthread:-lc_r}}						\
     %{pg:%{!pthread:%{!kthread:-lc_p}					\
       %{kthread:-lpthread_p -lc_p}}					\
       %{pthread:-lc_r_p}}}"


/* Code generation parameters.  */

/* Make gcc agree with <machine/ansi.h>.  */

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions
   (even though the SVR4 ABI for the i386 says that records and unions are
   returned in memory).  */
#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Ensure we the configuration knows our system correctly so we can link with
   libraries compiled with the native cc.  */
#undef NO_DOLLAR_IN_LABEL

/* Use more efficient ``thunks'' to implement C++ vtables.  */
#undef DEFAULT_VTABLE_THUNKS
#define DEFAULT_VTABLE_THUNKS 1

/* This is BSD, so use stabs instead of DWARF debug format.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Attach a special .ident directive to the end of the file to identify
   the version of GCC which compiled this code.  The format of the .ident
   string is patterned after the ones produced by native SVR4 C compilers.  */
#undef IDENT_ASM_OP
#define IDENT_ASM_OP ".ident"

/* Output #ident as a .ident.  */
#undef ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME)					\
  fprintf ((FILE), "\t%s\t\"%s\"\n", IDENT_ASM_OP, (NAME));

#undef ASM_IDENTIFY_LANGUAGE
#define ASM_IDENTIFY_LANGUAGE(FILE)					\
  fprintf ((FILE), "\t%s \"GCC (%s) %s\"\n", IDENT_ASM_OP,		\
	   lang_identify (), version_string)

#undef ASM_FILE_END
#define ASM_FILE_END(FILE)						\
do {				 					\
     if (!flag_no_ident)						\
	fprintf ((FILE), "\t%s\t\"GCC: (GNU) %s\"\n",			\
		 IDENT_ASM_OP, version_string);				\
   } while (0)


/* Miscellaneous parameters.  */

/* Don't assume anything about the header files.  */
#undef NO_IMPLICIT_EXTERN_C
#define NO_IMPLICIT_EXTERN_C

/* Allow #sccs in preprocessor.  */
#define SCCS_DIRECTIVE

/* Handle #pragma weak and #pragma pack.  */
#define HANDLE_SYSV_PRAGMA

/* Tell libgcc2.c that FreeBSD targets support atexit(3).  */
#define HAVE_ATEXIT
