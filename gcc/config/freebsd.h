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


/* To help with rs6000/sysv4.h carnal knowledge problem.  */
#define _USING_CONFIG_FREEBSD 1

/* This defines which switch letters take arguments.  On FreeBSD, most of
   the normal cases (defined in gcc.c) apply, and we also have -h* and
   -z* options (for the linker) (coming from SVR4).
   We also have -R (alias --rpath), no -z, --soname (-h), --assert etc.  */

#define FBSD_SWITCH_TAKES_ARG(CHAR)					\
  (DEFAULT_SWITCH_TAKES_ARG (CHAR)					\
    || (CHAR) == 'h'							\
    || (CHAR) == 'z' /* ignored by ld */				\
    || (CHAR) == 'R')

#undef  SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) (FBSD_SWITCH_TAKES_ARG(CHAR))

/* This defines which multi-letter switches take arguments.  */

#define FBSD_WORD_SWITCH_TAKES_ARG(STR)					\
  (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)					\
   || !strcmp ((STR), "rpath") || !strcmp ((STR), "rpath-link")		\
   || !strcmp ((STR), "soname") || !strcmp ((STR), "defsym") 		\
   || !strcmp ((STR), "assert") || !strcmp ((STR), "dynamic-linker"))

#undef  WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR) (FBSD_WORD_SWITCH_TAKES_ARG(STR))

#if FBSD_MAJOR == 6
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__=6 -Dunix -D__ELF__ -D__KPRINTF_ATTRIBUTE__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#if FBSD_MAJOR == 5
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__=5 -Dunix -D__ELF__ -D__KPRINTF_ATTRIBUTE__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#if FBSD_MAJOR == 4
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__=4 -Dunix -D__ELF__ -D__KPRINTF_ATTRIBUTE__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#if FBSD_MAJOR == 3
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__=3 -Dunix -D__ELF__ -D__KPRINTF_ATTRIBUTE__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#ifndef FBSD_CPP_PREDEFINES
#define FBSD_CPP_PREDEFINES \
  "-D__FreeBSD__   -Dunix -D__ELF__ -D__KPRINTF_ATTRIBUTE__ -Asystem=unix -Asystem=bsd -Asystem=FreeBSD"
#endif

#undef  CPP_PREDEFINES
#define CPP_PREDEFINES FBSD_CPP_PREDEFINES

/* Provide a CPP_SPEC appropriate for FreeBSD.  We just deal with the GCC 
   option `-posix', and PIC issues.  */

#define FBSD_CPP_SPEC "							\
  %(cpp_cpu)								\
  %{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__}		\
  %{posix:-D_POSIX_SOURCE}"

#undef  CPP_SPEC
#define CPP_SPEC FBSD_CPP_SPEC

/* Provide a STARTFILE_SPEC appropriate for FreeBSD.  Here we add
   the magical crtbegin.o file (see crtstuff.c) which provides part 
	of the support for getting C++ file-scope static object constructed 
	before entering `main'. */
   
#define FBSD_STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} \
		       %{!p:%{profile:gcrt1.o%s} \
			 %{!profile:crt1.o%s}}}} \
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

/* Provide a ENDFILE_SPEC appropriate for FreeBSD.  Here we tack on
   the magical crtend.o file (see crtstuff.c) which provides part of 
	the support for getting C++ file-scope static object constructed 
	before entering `main', followed by a normal "finalizer" file, 
	`crtn.o'.  */

#define FBSD_ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

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

/* Provide a LIB_SPEC appropriate for FreeBSD.  Just select the appropriate
   libc, depending on whether we're doing profiling or need threads support.
   (simular to the default, except no -lg, and no -p).  */

#ifdef FBSD_NO_THREADS
#define FBSD_LIB_SPEC "							\
  %{pthread: %eThe -pthread option is only supported on FreeBSD when gcc \
is built with the --enable-threads configure-time option.}		\
  %{!shared:								\
    %{!pg: -lc}								\
    %{pg:  -lc_p}							\
  }"
#else
#if FBSD_MAJOR >= 5
#define FBSD_LIB_SPEC "							\
  %{!shared:								\
    %{!pg: %{pthread:-lc_r} -lc}					\
    %{pg:  %{pthread:-lc_r_p} -lc_p}					\
  }"
#else
#define FBSD_LIB_SPEC "							\
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

#undef  LIB_SPEC
#define LIB_SPEC FBSD_LIB_SPEC


/************************[  Target stuff  ]***********************************/

/* Don't assume anything about the header files.  */
#undef  NO_IMPLICIT_EXTERN_C
#define NO_IMPLICIT_EXTERN_C

/* Allow #sccs in preprocessor.  */
#undef  SCCS_DIRECTIVE
#define SCCS_DIRECTIVE

/* Make gcc agree with FreeBSD's standard headers (<machine/ansi.h>, etc...)  */

#undef  WCHAR_TYPE
#define WCHAR_TYPE "int"

/* Code generation parameters.  */

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions
   (even though the SVR4 ABI for the i386 says that records and unions are
   returned in memory).  */
#undef  DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Use periods rather than dollar signs in special g++ assembler names.
   This ensures the configuration knows our system correctly so we can link
   with libraries compiled with the native cc.  */
#undef NO_DOLLAR_IN_LABEL

/* The prefix to add to user-visible assembler symbols.
   For System V Release 4 & ELF the convention is *not* to prepend a leading
   underscore onto user-level symbol names. Some CPU files such as
   config/sparc/sparc.h set this wrong for ELF.  */

#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* Handle #pragma weak and #pragma pack.  */
#undef  HANDLE_SYSV_PRAGMA
#define HANDLE_SYSV_PRAGMA

/* Use more efficient ``thunks'' to implement C++ vtables.  */
#undef DEFAULT_VTABLE_THUNKS
#define DEFAULT_VTABLE_THUNKS 1

/************************[  Assembler stuff  ]********************************/

#undef  IDENT_ASM_OP
#define IDENT_ASM_OP "\t.ident\t"

/* Output #ident as a .ident.  */

#undef  ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME)					\
  fprintf ((FILE), "%s\"%s\"\n", IDENT_ASM_OP, (NAME));

/************************[  Debugger stuff  ]*********************************/

/* All ELF targets can support DWARF-2.  */
#undef  DWARF2_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO

/* This is BSD, so we want the DBX format.  */
#undef  DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

/* Even though this is BSD, ELF and the GNU tools operates better with dwarf2
   than stabs.  Since we don't have any native tools to be compatible with,
   defaulting to dwarf2 is OK.  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
