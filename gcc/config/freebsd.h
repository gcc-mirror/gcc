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


/* In case we need to know.  */
#define USING_CONFIG_FREEBSD 1

/* This defines which switch letters take arguments.  On FreeBSD, most of
   the normal cases (defined in gcc.c) apply, and we also have -h* and
   -z* options (for the linker) (coming from SVR4).
   We also have -R (alias --rpath), no -z, --soname (-h), --assert etc.  */

#undef  SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) (FBSD_SWITCH_TAKES_ARG(CHAR))

#undef  WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR) (FBSD_WORD_SWITCH_TAKES_ARG(STR))

#undef  CPP_PREDEFINES
#define CPP_PREDEFINES FBSD_CPP_PREDEFINES

#undef  CPP_SPEC
#define CPP_SPEC FBSD_CPP_SPEC

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC FBSD_STARTFILE_SPEC

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC FBSD_ENDFILE_SPEC

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

#undef  WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

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

/* Used by libgcc2.c.  We support file locking with fcntl / F_SETLKW.
   This enables the test coverage code to use file locking when exiting a
   program, which avoids race conditions if the program has forked.  */
#define TARGET_HAS_F_SETLKW

/* The prefix to add to user-visible assembler symbols.
   For System V Release 4 & ELF the convention is *not* to prepend a leading
   underscore onto user-level symbol names. Some CPU files such as
   config/sparc/sparc.h set this wrong for ELF.  */

#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* Handle #pragma weak and #pragma pack.  */
#undef  HANDLE_SYSV_PRAGMA
#define HANDLE_SYSV_PRAGMA

/************************[  Assembler stuff  ]********************************/

#undef  IDENT_ASM_OP
#define IDENT_ASM_OP "\t.ident\t"

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
