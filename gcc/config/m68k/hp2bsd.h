/* Definitions of target machine for GNU compiler.  HP 9000/200 68000 version.
   Copyright (C) 1987, 1991 Free Software Foundation, Inc.

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

#include "m68k/m68k.h"

/* See m68k.h.  0 means 68000 with no 68881.  */

#define TARGET_DEFAULT 0

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* Define __HAVE_68881 in preprocessor only if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros.
   Also inform the program which CPU this is for.  */

#define CPP_SPEC "%{m68881:-D__HAVE_68881__} \
%{!ansi:%{m68020:-Dmc68020}%{mc68020:-Dmc68020}%{!mc68020:%{!m68020:-Dmc68010}}}"

/* -m68020 requires special flags to the assembler.  */

#define ASM_SPEC \
 "%{m68020:-mc68020}%{mc68020:-mc68020}%{!mc68020:%{!m68020:-mc68010}}"

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dmc68000 -Dmc68010 -Dhp200 -Dunix -Asystem=unix -Asystem=bsd -Acpu=m68k -Amachine=m68k"

/* Link with libg.a when debugging, for dbx's sake.  */

#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} "

/* Alignment of field after `int : 0' in a structure.  */

#undef EMPTY_FIELD_BOUNDARY
#define EMPTY_FIELD_BOUNDARY 16

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO

/* Define subroutines to call to handle multiply, divide, and remainder.
   These routines are built into the c-library on the hp200.
   XXX What other routines from the c-library could we use?
   The `*' prevents an underscore from being prepended by the compiler.  */

#define DIVSI3_LIBCALL "*ldiv"
#define UDIVSI3_LIBCALL "*uldiv"
#define MODSI3_LIBCALL "*lrem"
#define UMODSI3_LIBCALL "*ulrem"
#define MULSI3_LIBCALL "*lmul"
#define UMULSI3_LIBCALL "*ulmul"

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
