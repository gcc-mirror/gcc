/* Definitions of target machine for GNU compiler.  DECstation (OSF/1) version.
   Copyright (C) 1992 Free Software Foundation, Inc.

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

#define DEC_OSF1

#define CPP_PREDEFINES "-D__ANSI_COMPAT \
-DMIPSEL -DR3000 -DSYSTYPE_BSD -D_SYSTYPE_BSD -Dbsd4_2 -Dhost_mips -Dmips -Dosf -Dunix"

#define LINK_SPEC "\
%{G*} \
%{!mgas: \
	%{EL} %{!EL: -EL} \
	%{EB: %e-EB not supported} \
	%{mips1} %{mips2} %{mips3} %{bestGnum} \
	%{shared} %{non_shared} %{call_shared} %{no_archive} %{exact_version} \
	%{!shared: %{!non_shared: %{!call_shared: -non_shared}}}}"

#include "decstatn.h"

/* Specify size_t, ptrdiff_t, and wchar_t types.  */
#undef	SIZE_TYPE
#undef	PTRDIFF_TYPE
#undef	WCHAR_TYPE
#undef	WCHAR_TYPE_SIZE

#define SIZE_TYPE	"long unsigned int"
#define PTRDIFF_TYPE	"int"
#define WCHAR_TYPE	"short unsigned int"
#define WCHAR_TYPE_SIZE SHORT_TYPE_SIZE

/* turn off collect2 COFF support, since ldfcn now has elf declaration */
#undef OBJECT_FORMAT_COFF

#undef MACHINE_TYPE
#define MACHINE_TYPE "DECstation running DEC OSF/1"
