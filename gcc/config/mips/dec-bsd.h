/* Definitions for DECstation running BSD as target machine for GNU compiler.
   Copyright (C) 1993, 1995 Free Software Foundation, Inc.

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

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ANSI_COMPAT \
-DMIPSEL -DR3000 -DSYSTYPE_BSD -D_SYSTYPE_BSD -Dbsd4_4 -Dhost_mips -Dmips \
-Dunix -D_mips -D_unix -D_host_mips -D_MIPSEL -D_R3000 \
-Asystem(unix) -Asystem(bsd) -Amachine(mips)"
#endif

/* Always uses gas.  */
#ifndef ASM_SPEC
#define ASM_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{v} \
%{noasmopt:-O0} \
%{!noasmopt:%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}} \
%{g} %{g0} %{g1} %{g2} %{g3} \
%{ggdb:-g} %{ggdb0:-g0} %{ggdb1:-g1} %{ggdb2:-g2} %{ggdb3:-g3} \
%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3} \
%{gcoff:-g} %{gcoff0:-g0} %{gcoff1:-g1} %{gcoff2:-g2} %{gcoff3:-g3}"
#endif

#ifndef CPP_SPEC
#define CPP_SPEC "\
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C__ -D__LANGUAGE_OBJECTIVE_C} \
%{.S:	-D__LANGUAGE_ASSEMBLY__ -D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.s:	-D__LANGUAGE_ASSEMBLY -D_LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{!.S:%{!.s:	-D__LANGUAGE_C -D_LANGUAGE_C %{!ansi:-DLANGUAGE_C}}} \
%{mlong64:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int} \
%{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int} \
%{mips3:-U__mips -D__mips=3 -D__mips64} \
%{mgp32:-U__mips64} %{mgp64:-D__mips64} \
%{EB:-UMIPSEL -U_MIPSEL -U__MIPSEL -U__MIPSEL__ -D_MIPSEB -D__MIPSEB -D__MIPSEB__ %{!ansi:-DMIPSEB}} \
%{EL:-UMIPSEB -U_MIPSEB -U__MIPSEB -U__MIPSEB__ -D_MIPSEL -D__MIPSEL -D__MIPSEL__ %{!ansi:-DMIPSEL}}"
#endif

/* Always uses GNU ld.  */
#ifndef LINK_SPEC
#define LINK_SPEC "%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3}"
#endif

#define LIB_SPEC ""
#define STARTFILE_SPEC ""

#ifndef MACHINE_TYPE
#define MACHINE_TYPE "DECstation running BSD 4.4"
#endif

#define TARGET_DEFAULT MASK_GAS
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#include "mips/mips.h"

/* Since gas and gld are standard on 4.4 BSD, we don't need these */
#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX
#undef ASM_FINAL_SPEC
#undef LIB_SPEC
#undef STARTFILE_SPEC

