/* Definitions of target machine for GNU compiler.  MIPS GNU Hurd version.
   Copyright (C) 1994 Free Software Foundation, Inc.

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

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dmips -DMACH -D__GNU__ -D__HURD__ \
-Asystem(unix) -Asystem(mach) -Acpu(mips) -Amachine(mips) -Asystem(gnu) \

#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared}"

#define MACHINE_TYPE "MIPS-GNU"

#include "mips/mips.h"

#undef ASM_FINAL_SPEC
#undef SDB_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO

/* Generate DBX debugging information.  */
#define DBX_DEBUGGING_INFO

#undef ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(STREAM,LINE) \
    fprintf (STREAM, "\t.stabd %d,0,%d\n", \
	     N_SLINE, LINE);

/* We use BSD object format and our assembler handles .stab<x>. */
#undef ASM_STABS_OP
#undef ASM_STABN_OP
#undef ASM_STABD_OP
#define ASM_STABS_OP	(".stabs")
#define ASM_STABN_OP	(".stabn")
#define ASM_STABD_OP	(".stabd")
