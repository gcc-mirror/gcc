/* Base configuration file for all FreeBSD targets.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

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
   David O'Brien <obrien@FreeBSD.org>.  
   Further work by David O'Brien <obrien@FreeBSD.org> and
   Loren J. Rittle <ljrittle@acm.org>.  */


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

#if FBSD_MAJOR == 6
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__=6 -Dunix -D__ELF__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#if FBSD_MAJOR == 5
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__=5 -Dunix -D__ELF__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#if FBSD_MAJOR == 4
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__=4 -Dunix -D__ELF__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#if FBSD_MAJOR == 3
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__=3 -Dunix -D__ELF__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#ifndef FBSD_CPP_PREDEFINES
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__   -Dunix -D__ELF__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#undef  CPP_PREDEFINES
#define CPP_PREDEFINES FBSD_CPP_PREDEFINES

/* Provide a CPP_SPEC appropriate for FreeBSD.  We just deal with the GCC 
   option `-posix', and PIC issues.  */

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu)						\
  %{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__}		\
  %{posix:-D_POSIX_SOURCE}"

/* Provide a LIB_SPEC appropriate for FreeBSD as configured and as
   required by the user-land thread model.  Before __FreeBSD_version
   500016, select the appropriate libc, depending on whether we're
   doing profiling or need threads support.  At __FreeBSD_version
   500016 and later, when threads support is requested include both
   -lc and -lc_r instead of only -lc_r.  To make matters interesting,
   we can't actually use __FreeBSD_version provided by <osreldate.h>
   directly since it breaks cross-compiling.  As a final twist, make
   it a hard error if -pthread is provided on the command line and gcc
   was configured with --disable-threads (this will help avoid bug
   reports from users complaining about threading when they
   misconfigured the gcc bootstrap but are later consulting FreeBSD
   manual pages that refer to the mythical -pthread option).  */

#undef  LIB_SPEC
#ifdef FBSD_NO_THREADS
#define LIB_SPEC "							\
  %{pthread: %eThe -pthread option is only supported on FreeBSD when gcc \
is built with the --enable-threads configure-time option.}		\
  %{!shared:								\
    %{!pg: -lc}								\
    %{pg:  -lc_p}							\
  }"
#else
#if FBSD_MAJOR >= 5
#define LIB_SPEC "							\
  %{!shared:								\
    %{!pg: %{pthread:-lc_r} -lc}					\
    %{pg:  %{pthread:-lc_r_p} -lc_p}					\
  }"
#else
#define LIB_SPEC "							\
  %{!shared:								\
    %{!pg:								\
      %{!pthread:-lc}							\
      %{pthread:-lc_r}}							\
    %{pg:								\
      %{!pthread:-lc_p}							\
      %{pthread:-lc_r_p}}						\
  }"
#endif
#endif

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

/* The GNU tools operate better with dwarf2 than stabs.  Since we
   don't have any native tools to be compatible with, default to
   dwarf2.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef IDENT_ASM_OP
#define IDENT_ASM_OP "\t.ident\t"

/* Output #ident as a .ident.  */
#undef ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME)					\
  fprintf ((FILE), "%s\"%s\"\n", IDENT_ASM_OP, (NAME));

/* Miscellaneous parameters.  */

/* Don't assume anything about the header files.  */
#undef NO_IMPLICIT_EXTERN_C
#define NO_IMPLICIT_EXTERN_C

/* Allow #sccs in preprocessor.  */
#define SCCS_DIRECTIVE

/* Handle #pragma weak and #pragma pack.  */
#define HANDLE_SYSV_PRAGMA
