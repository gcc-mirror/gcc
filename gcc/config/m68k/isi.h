/* Definitions of target machine for GNU compiler.  ISI 68000/68020 version.
   Intended only for use with GAS, and not ISI's assembler, which is buggy
   Copyright (C) 1988 Free Software Foundation, Inc.

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

/* See m68k.h.  7 means 68020 with 68881. */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 7
#endif

#if TARGET_DEFAULT & 2
/* Define __HAVE_68881__ in preprocessor, unless -msoft-float is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#define CPP_SPEC "%{!msoft-float:-D__HAVE_68881__}"

/* If the 68881 is used, link must load libmc.a before libc.a.  */

#define LIB_SPEC "%{!msoft-float:%{!p:%{!pg:-lmc}}%{p:-lmc_p}%{pg:-lmc_p}} \
%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} %{g:-lg}"

#else
/* Define __HAVE_68881__ in preprocessor if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#define CPP_SPEC "%{m68881:-D__HAVE_68881__}"

/* If the 68881 is used, link must load libmc.a instead of libc.a */

#define LIB_SPEC "%{m68881:%{!p:%{!pg:-lmc}}%{p:-lmc_p}%{pg:-lmc_p}} \
%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} %{g:-lg}"
#endif

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dunix -Dmc68000 -Dis68k -Asystem(unix) -Asystem(bsd) -Acpu(m68k) -Amachine(m68k)"

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO

/* Override parts of m68k.h to fit the ISI 68k machine.  */

#undef FUNCTION_VALUE
#undef LIBCALL_VALUE
#undef FUNCTION_VALUE_REGNO_P
#undef NEEDS_UNTYPED_CALL
#undef ASM_FILE_START

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* If TARGET_68881, return SF and DF values in f0 instead of d0.  */

#define FUNCTION_VALUE(VALTYPE,FUNC) LIBCALL_VALUE (TYPE_MODE (VALTYPE))

#define LIBCALL_VALUE(MODE) \
 gen_rtx (REG, (MODE), ((TARGET_68881 && ((MODE) == SFmode || (MODE) == DFmode)) ? 16 : 0))

/* 1 if N is a possible register number for a function value.
   D0 may be used, and F0 as well if -m68881 is specified.  */

#define FUNCTION_VALUE_REGNO_P(N) \
 ((N) == 0 || (TARGET_68881 && (N) == 16))

/* Define this to be true when FUNCTION_VALUE_REGNO_P is true for
   more than one register.  */

#define NEEDS_UNTYPED_CALL 1

/* Also output something to cause the correct _doprnt to be loaded.  */
#define ASM_FILE_START(FILE) fprintf (FILE, "#NO_APP\n%s\n", TARGET_68881 ? ".globl fltused" : "")
