/* Definitions of target machine for GNU compiler.  DECstation (OSF/1 with OSF/rose) version.
   Copyright (C) 1991 Free Software Foundation, Inc.

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

#include "halfpic.h"

#define CPP_PREDEFINES "-DOSF -DOSF1 -Dbsd4_2 -DMIPSEL -Dhost_mips -Dmips -Dunix -DR3000 -DSYSTYPE_BSD"

#define ASM_SPEC	"\
%{mmips-as: \
	%{pipe:%e:-pipe not supported} \
	%{EL} %{!EL:-EL} \
	%{EB: %e-EB not supported} \
	%{!mrnames: -nocpp} \
	%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3} \
	%{g} %{g0} %{g1} %{g2} %{g3} \
	%{K} %{Q}} \
	%{v*: -v} \
	%{G*}"

#define ASM_FINAL_SPEC "\
%{mmips-as: %{!mno-mips-tfile: \
	\n mips-tfile %{v*: -v} %{d*} \
			%{K: -I %b.o~} \
			%{!K: %{save-temps: -I %b.o~}} \
			%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %b.o} \
			%{.s:%i} %{!.s:%g.s}}}"

#define CPP_SPEC "\
%{.S:	-D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C} \
%{!.S:	-D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}"

#define LINK_SPEC "\
%{G*} \
%{mmips-as: \
	%{EL} %{!EL: -EL} \
	%{EB: %e-EB not supported} \
	%{bestGnum}} \
%{!mmips-as: \
 	%{v*: -v} \
	%{pic-none: -noshrlib} %{noshrlib} \
	%{!pic-none: -warn_nopic} \
	%{nostdlib} %{glue}}"

/* For now, force static libraries instead of shared, but do so that
   does not use -noshrlib, since the old linker does not provide it.  */
#define LIB_SPEC "%{!pic-none: %{!pic-lib: -L/usr/ccs/lib }} -lc"

#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

#define MACHINE_TYPE "DECstation running OSF/1 with OSF/rose objects"

#ifndef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX		"/usr/ccs/gcc/"
#endif

#ifndef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX	"/usr/ccs/lib/"
#endif

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

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

#include "mips.h"
