/* Definitions of target machine for GNU compiler.
   DECstation (OSF/1 reference port with OSF/rose) version.
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define DECSTATION
#define OSF_OS

#define HALF_PIC_DEBUG	TARGET_DEBUG_B_MODE
#define HALF_PIC_PREFIX	"$Lp."

#include "halfpic.h"

#define WORD_SWITCH_TAKES_ARG(STR)					\
 (!strcmp (STR, "Tdata") || !strcmp (STR, "Ttext")			\
  || !strcmp (STR, "Tbss") || !strcmp (STR, "include")			\
  || !strcmp (STR, "imacros") || !strcmp (STR, "aux-info")		\
  || !strcmp (STR, "pic-names"))

#define CPP_PREDEFINES "-DOSF -DOSF1 -Dbsd4_2 -DMIPSEL -Dhost_mips -Dmips -Dunix -DR3000 -DSYSTYPE_BSD"

#define ASM_SPEC	"\
%{mmips-as: \
	%{pipe:%e:-pipe not supported} \
	%{EL} %{!EL:-EL} \
	%{EB: %e-EB not supported} \
	%{!mrnames: %{!.s:-nocpp} %{.s: %{cpp} %{nocpp}}} \
	%{mips1} %{mips2} %{mips3} \
	%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3} \
	%{g} %{g0} %{g1} %{g2} %{g3} \
	%{K} %{Q}} \
	%{v*: -v} \
	%{G*}"

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
%{.S:	-D__LANGUAGE_ASSEMBLY__ -D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY} \
	-ULANGUAGE_C -U__LANGUAGE_C__} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C__ -D__LANGUAGE_OBJECTIVE_C} \
%{!.S:	-D__LANGUAGE_C__  -D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}"

#define LINK_SPEC "\
%{G*} \
%{mmips-as: \
	%{EL} %{!EL: -EL} \
	%{EB: %e-EB not supported} \
	%{mips1} %{mips2} %{mips3} \
	%{bestGnum}} \
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
%{O*: %{!mno-gpOPT:%{!mno-gpopt: -mgpopt}}} \
%{gline:%{!g:%{!g0:%{!g1:%{!g2: -g1}}}}} \
%{G*} \
%{pic-none:   -mno-half-pic} \
%{pic-lib:    -mhalf-pic} \
%{pic-extern: -mhalf-pic} \
%{pic-calls:  -mhalf-pic} \
%{pic-names*: -mhalf-pic} \
%{!pic-*:     -mhalf-pic}"

/* Specify size_t, ptrdiff_t, and wchar_t types.  */
#define SIZE_TYPE	"long unsigned int"
#define PTRDIFF_TYPE	"int"
#define WCHAR_TYPE	"unsigned int"
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#if 0
#define WCHAR_TYPE	((TARGET_WC8)					\
				? "unsigned char"			\
				: ((TARGET_WC16)			\
					? "short unsigned int"		\
					: "long unsigned int"))
#endif

#define TARGET_DEFAULT MASK_GAS

/* OSF/rose uses stabs, not ECOFF.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Tell collect that the object format is OSF/rose.  */
#define OBJECT_FORMAT_ROSE

/* Tell collect where the appropriate binaries are.  */
#define REAL_LD_FILE_NAME	"/usr/ccs/gcc/gld"
#define REAL_NM_FILE_NAME	"/usr/ccs/bin/nm"
#define REAL_STRIP_FILE_NAME	"/usr/ccs/bin/strip"

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

#include "mips.h"
