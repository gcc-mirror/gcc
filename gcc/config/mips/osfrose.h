/* Definitions of target machine for GNU compiler.
   DECstation (OSF/1 reference port with OSF/rose) version.
   Copyright (C) 1991, 1992, 1995 Free Software Foundation, Inc.

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

#define DECSTATION
#define OSF_OS

#define HALF_PIC_DEBUG	TARGET_DEBUG_B_MODE
#define HALF_PIC_PREFIX	"$Lp."

#include "halfpic.h"

#define WORD_SWITCH_TAKES_ARG(STR)					\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR) || !strcmp (STR, "pic-names"))

#define CPP_PREDEFINES "\
-DOSF -DOSF1 -Dbsd4_2 -DMIPSEL -Dhost_mips -Dmips -Dunix -DR3000 -DSYSTYPE_BSD \
-Asystem(unix) -Asystem(xpg4) -Acpu(mips) -Amachine(mips)"

#define ASM_SPEC "\
%{mmips-as: \
	%{!.s:-nocpp} %{.s: %{cpp} %{nocpp}} \
	%{pipe: %e-pipe is not supported.} \
	%{K}} \
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{v} \
%{noasmopt:-O0} \
%{!noasmopt:%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}} \
%{g} %{g0} %{g1} %{g2} %{g3} \
%{ggdb:-g} %{ggdb0:-g0} %{ggdb1:-g1} %{ggdb2:-g2} %{ggdb3:-g3} \
%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3} \
%{gcoff:-g} %{gcoff0:-g0} %{gcoff1:-g1} %{gcoff2:-g2} %{gcoff3:-g3}"

#ifndef CROSS_COMPILE
#define ASM_FINAL_SPEC "\
%{mmips-as: %{!mno-mips-tfile: \
	\n mips-tfile %{v*: -v} %{d*} \
			%{K: -I %b.o~} \
			%{!K: %{save-temps: -I %b.o~}} \
			%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
			%{.s:%i} %{!.s:%g.s}}}"

#else				/* CROSS_COMPILE */
#define ASM_FINAL_SPEC "\
%{mmips-as: %{mmips-tfile: \
	\n mips-tfile %{v*: -v} %{d*} \
			%{K: -I %b.o~} \
			%{!K: %{save-temps: -I %b.o~}} \
			%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
			%{.s:%i} %{!.s:%g.s}}}"
#endif

#define CPP_SPEC "\
%{.S:	%{!ansi:%{!traditional:%{!traditional-cpp:%{!ftraditional: -traditional}}}}} \
%{.S:	-D__LANGUAGE_ASSEMBLY__ -D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY} \
	-ULANGUAGE_C -U__LANGUAGE_C__} \
%{.s:	%{!ansi:%{!traditional:%{!traditional-cpp:%{!ftraditional: -traditional}}}}} \
%{.s:	-D__LANGUAGE_ASSEMBLY__ -D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY} \
	-ULANGUAGE_C -U__LANGUAGE_C__} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C__ -D__LANGUAGE_OBJECTIVE_C} \
%{!.S:%{!.s:	-D__LANGUAGE_C__  -D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}} \
%{mlong64:-D__PTRDIFF_TYPE__=long\\ int} \
%{!mlong64:-D__PTRDIFF_TYPE__=int} \
%{mips3:-U__mips -D__mips=3 -D__mips64} \
%{mgp32:-U__mips64} %{mgp64:-D__mips64} \
%{EB:-UMIPSEL -U_MIPSEL -U__MIPSEL -U__MIPSEL__ -D_MIPSEB -D__MIPSEB -D__MIPSEB__ %{!ansi:-DMIPSEB}} \
%{EL:-UMIPSEB -U_MIPSEB -U__MIPSEB -U__MIPSEB__ -D_MIPSEL -D__MIPSEL -D__MIPSEL__ %{!ansi:-DMIPSEL}}"

/* ??? This assumes that GNU as is always used with GNU ld, and MIPS as is
   always used with MIPS ld.  */
#define LINK_SPEC "\
%{G*} %{EL} %{EB} %{mips1} %{mips2} %{mips3} \
%{bestGnum} \
%{!mmips-as: \
 	%{v*: -v} \
	%{!noshrlib: %{pic-none: -noshrlib} %{!pic-none: -warn_nopic}} \
	%{nostdlib} %{noshrlib} %{glue}}"

#define LIB_SPEC "-lc"

/* Define this macro meaning that `gcc' should find the library
   `libgcc.a' by hand, rather than passing the argument `-lgcc' to
   tell the linker to do the search. */

#define LINK_LIBGCC_SPECIAL 1

#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

#define MACHINE_TYPE "DECstation with OSF/rose objects"

#ifndef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX		"/usr/ccs/gcc/"
#endif

#ifndef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX	"/usr/ccs/lib/"
#endif

/* Turn on -mpic-extern by default.  */
#define CC1_SPEC "\
%{gline:%{!g:%{!g0:%{!g1:%{!g2: -g1}}}}} \
%{mips1:-mfp32 -mgp32}%{mips2:-mfp32 -mgp32}%{mips3:-mfp64 -mgp64} \
%{G*} \
%{pic-none:   -mno-half-pic} \
%{pic-lib:    -mhalf-pic} \
%{pic-extern: -mhalf-pic} \
%{pic-calls:  -mhalf-pic} \
%{pic-names*: -mhalf-pic} \
%{!pic-*:     -mhalf-pic}"

/* Specify size_t and wchar_t types.  */
#define SIZE_TYPE	"long unsigned int"
#define WCHAR_TYPE	"unsigned int"
#define WCHAR_TYPE_SIZE BITS_PER_WORD
#define MAX_WCHAR_TYPE_SIZE MAX_LONG_TYPE_SIZE

/* OSF/1 uses gas, not the mips assembler.  */
#define TARGET_DEFAULT MASK_GAS

/* OSF/rose uses stabs, not ECOFF.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* enable dwarf debugging for testing */
#define DWARF_DEBUGGING_INFO
/* This is needed by dwarfout.c.  */
#define SET_ASM_OP	".set"

/* Tell collect that the object format is OSF/rose.  */
#define OBJECT_FORMAT_ROSE

/* Tell collect where the appropriate binaries are.  */
#define REAL_LD_FILE_NAME	"/usr/ccs/gcc/gld"
#define REAL_NM_FILE_NAME	"/usr/ccs/bin/nm"
#define REAL_STRIP_FILE_NAME	"/usr/ccs/bin/strip"

/* Default to -G 0 unless doing ecoff work.  */
#define MIPS_DEFAULT_GVALUE ((TARGET_MIPS_AS) ? 8 : 0)

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

/* Generate calls to memcpy, etc., not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

/* A C statement to output assembler commands which will identify
   the object file as having been compiled with GNU CC (or another
   GNU compiler).

   If you don't define this macro, the string `gcc2_compiled.:' is
   output.  This string is calculated to define a symbol which, on
   BSD systems, will never be defined for any other reason.  GDB
   checks for the presence of this symbol when reading the symbol
   table of an executable.

   On non-BSD systems, you must arrange communication with GDB in
   some other fashion.  If GDB is not used on your system, you can
   define this macro with an empty body.

   On OSF/1, gcc2_compiled. confuses the kernel debugger, so don't
   put it out.  */

#define ASM_IDENTIFY_GCC(STREAM)

/* Identify the front-end which produced this file.  To keep symbol
   space down, and not confuse kdb, only do this if the language is
   not C.  */

#define ASM_IDENTIFY_LANGUAGE(STREAM)					\
{									\
  if (strcmp (lang_identify (), "c") != 0)				\
    output_lang_identify (STREAM);					\
}

#include "mips/mips.h"
