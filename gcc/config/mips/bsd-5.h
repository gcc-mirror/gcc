/* Definitions of target machine for GNU compiler.  MIPS RISC-OS, 5.0 BSD version.
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

#define MIPS_BSD43

#define CPP_PREDEFINES "\
-Dmips -Dunix -Dhost_mips -DMIPSEB -DR3000 -DSYSTYPE_BSD43 \
-D_mips -D_unix -D_host_mips -D_MIPSEB -D_R3000 -D_SYSTYPE_BSD43 \
-Asystem(unix) -Asystem(bsd) -Acpu(mips) -Amachine(mips)"

#define STANDARD_INCLUDE_DIR "/bsd43/usr/include"

#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{!shared: %{!non_shared: %{!call_shared: -non_shared}}} \
-systype /bsd43/"
		    
#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} -lc"

#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt1.o%s crtn.o%s}}"

#define MACHINE_TYPE "RISC-OS BSD Mips"

/* Generate calls to memcpy, etc., not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

/* Override defaults for finding the MIPS tools.  */
#define MD_STARTFILE_PREFIX "/bsd43/usr/lib/cmplrs/cc/"
#define MD_EXEC_PREFIX "/bsd43/usr/lib/cmplrs/cc/"

#include "mips/mips.h"

/* Some assemblers have a bug that causes backslash escaped chars in .ascii
   to be misassembled, so we just completely avoid it.  */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)			\
do {							\
  unsigned char *s;					\
  int i;						\
  for (i = 0, s = (unsigned char *)(PTR); i < (LEN); s++, i++)	\
    {							\
      if ((i % 8) == 0)					\
	fputs ("\n\t.byte\t", (FILE));			\
      fprintf ((FILE), "%s0x%x", (i%8?",":""), (unsigned)*s); \
    }							\
  fputs ("\n", (FILE));					\
} while (0)
