/* Definitions of target machine for GNU compiler.  DECstation (OSF/1) version.
   Copyright (C) 1992, 1996, 1998 Free Software Foundation, Inc.

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

#define DEC_OSF1

#define CPP_PREDEFINES "\
-D__ANSI_COMPAT -DMIPSEL -DR3000 -DSYSTYPE_BSD -D_SYSTYPE_BSD \
-Dbsd4_2 -Dhost_mips -Dmips -Dosf -Dunix \
-Asystem=unix -Asystem=xpg4 -Acpu=mips -Amachine=mips"

#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{!shared: %{!non_shared: %{!call_shared: -non_shared}}}"

#include "mips/ultrix.h"
#include "mips/mips.h"

/* Specify size_t and wchar_t types.  */
#undef	SIZE_TYPE
#undef	WCHAR_TYPE
#undef	WCHAR_TYPE_SIZE

#define SIZE_TYPE	"long unsigned int"
#define WCHAR_TYPE	"short unsigned int"
#define WCHAR_TYPE_SIZE SHORT_TYPE_SIZE

#undef SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mlong64:-D__PTRDIFF_TYPE__=long\\ int} \
%{!mlong64:-D__PTRDIFF_TYPE__=int}"

/* turn off collect2 COFF support, since ldfcn now has elf declaration */
#undef OBJECT_FORMAT_COFF

#undef MACHINE_TYPE
#define MACHINE_TYPE "DECstation running DEC OSF/1"
