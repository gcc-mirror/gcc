/* Definitions of target machine for GNU compiler.  MIPS RISC-OS BSD version.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define MIPS_BSD43

#define CPP_PREDEFINES "\
-Dmips -Dunix -Dhost_mips -DMIPSEB -DR3000 -DSYSTYPE_BSD43 \
-D_mips -D_unix -D_host_mips -D_MIPSEB -D_R3000 -D_SYSTYPE_BSD43 \
-Asystem=unix -Asystem=bsd -Acpu=mips -Amachine=mips"

#define STANDARD_INCLUDE_DIR "/bsd43/usr/include"

#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
-systype /bsd43/"
		    
#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} -lc"

#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt1.o%s crtn.o%s}}"

#define MACHINE_TYPE "RISC-OS BSD Mips"

/* Generate calls to memcpy, etc., not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

/* Override defaults for finding the MIPS tools.  */
#define MD_STARTFILE_PREFIX "/bsd43/usr/lib/cmplrs/cc/"
#define MD_EXEC_PREFIX "/bsd43/usr/lib/cmplrs/cc/"
