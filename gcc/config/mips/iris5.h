/* Definitions of target machine for GNU compiler.  Iris version 5.
   Copyright (C) 1993 Free Software Foundation, Inc.

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

#define	TARGET_DEFAULT	MASK_ABICALLS
#define ABICALLS_ASM_OP ".option pic2"

#define OBJECT_FORMAT_ELF

#include "mips/iris4.h"

/* Specify size_t, ptrdiff_t, and wchar_t types.  */
#undef	SIZE_TYPE
#undef	PTRDIFF_TYPE
#undef	WCHAR_TYPE
#undef	WCHAR_TYPE_SIZE

#define SIZE_TYPE	"unsigned int"
#define PTRDIFF_TYPE	"int"
#define WCHAR_TYPE	"long int"
#define WCHAR_TYPE_SIZE	LONG_TYPE_SIZE

/* ??? _MIPS_SIM and _MIPS_SZPTR should eventually depend on options when
   options for them exist.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-Dunix -Dmips -Dsgi -Dhost_mips -DMIPSEB -DSYSTYPE_SVR4 \
  -D_SVR4_SOURCE -D_MODERN_C -D__DSO__ \
  -D_MIPS_SIM=_MIPS_SIM_ABI32 -D_MIPS_SZPTR=32 \
  -Asystem(unix) -Asystem(svr4) -Acpu(mips) -Amachine(sgi)"

#undef CPP_SPEC
#define CPP_SPEC "\
%{!ansi:-D__EXTENSIONS__ -D_SGI_SOURCE -D_LONGLONG} \
%{.S:	-D_LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D_LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D_LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D_LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D_LANGUAGE_OBJECTIVE_C} \
%{!.S: %{!.cc: %{!.cxx: %{!.C: %{!.m: -D_LANGUAGE_C %{!ansi:-DLANGUAGE_C}}}}}}\
%{!mfp64: -D_MIPS_FPSET=16}%{mfp64: -D_MIPS_FPSET=32} \
%{mips1: -D_MIPS_ISA=_MIPS_ISA_MIPS1} \
%{mips2: -D_MIPS_ISA=_MIPS_ISA_MIPS2} \
%{mips3: -D_MIPS_ISA=_MIPS_ISA_MIPS3} \
%{!mips1: %{!mips2: %{!mips3: -D_MIPS_ISA=_MIPS_ISA_MIPS1}}} \
%{!mint64: -D_MIPS_SZINT=32}%{mint64: -D_MIPS_SZINT=64} \
%{!mlong64: -D_MIPS_SZLONG=32}%{mlong64: -D_MIPS_SZLONG=64}"

#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} \
%{!mgas: \
	%{mips1} %{mips2} %{mips3} %{bestGnum} \
	%{shared} %{non_shared} %{call_shared} %{no_archive} %{exact_version} \
	%{!shared: %{!non_shared: \
		   %{!call_shared: -call_shared -no_unresolved}}} \
	-_SYSTYPE_SVR4 }"

#undef LIB_SPEC
#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} -lc crtn.o%s"

/* We do not want to run mips-tfile!  */
#undef ASM_FINAL_SPEC

#undef OBJECT_FORMAT_COFF

/* We don't support debugging info for now. */
#undef DBX_DEBUGGING_INFO
#undef SDB_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO

#undef MACHINE_TYPE
#define MACHINE_TYPE "SGI running IRIX 5.0"

