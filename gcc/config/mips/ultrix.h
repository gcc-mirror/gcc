/* Definitions of target machine for GNU compiler.  DECstation (ultrix) version.
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

#define CPP_PREDEFINES "-D__ANSI_COMPAT \
-D__LANGUAGE_C -D__MIPSEL -D__R3000 -D__SYSTYPE_BSD -D__bsd4_2 -D__host_mips -D__mips -D__ultrix -D__unix \
-DLANGUAGE_C -DMIPSEL -DR3000 -DSYSTYPE_BSD -Dbsd4_2 -Dhost_mips -Dmips -Dultrix -Dunix"

#define ASM_SPEC	"%{!mgas:					\
				%{!mrnames: -nocpp}			\
				%{pipe:%e:-pipe not supported}		\
				%{EL} %{!EL:-EL}			\
				%{EB: %e-EB not supported}		\
				%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}	\
				%{g} %{g0} %{g1} %{g2} %{g3} %{v} %{K}}	\
			 %{G*}"

#define CPP_SPEC	"%{.S:	-D__LANGUAGE_ASSEMBLY__			\
				-D_LANGUAGE_ASSEMBLY			\
				%{!ansi:-DLANGUAGE_ASSEMBLY}}		\
			 %{!.S:	-D__LANGUAGE_C__			\
				-D_LANGUAGE_C				\
				%{!ansi:-DLANGUAGE_C}}"

#define LINK_SPEC	"%{G*}						\
			 %{!mgas:					\
				%{EL} %{!EL: -EL}			\
				%{EB: %e-EB not supported}		\
				%{bestGnum}}"

#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} -lc"

#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

/* For compatibility with types.h.  */
#define SIZE_TYPE "unsigned int"

#define MACHINE_TYPE "DECstation running ultrix"

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  Ultrix 4.x has this, 3.x probably does not.  */
#define HAVE_ATEXIT

#include "mips.h"
