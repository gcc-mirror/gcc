/* Definitions of target machine for GNU compiler.
   MIPS RISC-OS 5.0 System V.4 version.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define MIPS_SVR4

#define CPP_PREDEFINES \
"-Dmips -Dunix -Dhost_mips -DMIPSEB -DR3000 -DSYSTYPE_SVR4 \
-D_mips -D_unix -D_host_mips -D_MIPSEB -D_R3000 -D_SYSTYPE_SVR4 \
-D_MIPS_SZINT=32 -D_MIPS_SZLONG=32 -D_MIPS_SZPTR=32 \
-Asystem(unix) -Asystem(svr4) -Acpu(mips) -Amachine(mips)"

#define STANDARD_INCLUDE_DIR "/svr4/usr/include"

#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{!shared: %{!non_shared: %{!call_shared: -non_shared}}} \
-systype /svr4/ "
		    
#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} -lc crtn.o%s"

#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt1.o%s}}\
	%{ansi:/svr4/usr/ccs/lib/values-Xc.o%s} \
                          %{!ansi: \
                           %{traditional:/svr4/usr/ccs/lib/values-Xt.o%s} \
                           %{!traditional:/svr4/usr/ccs/lib/values-Xa.o%s}}"

#define MACHINE_TYPE "RISC-OS System V.4 Mips"

/* Override defaults for finding the MIPS tools.  */
#define MD_STARTFILE_PREFIX "/svr4/usr/lib/cmplrs/cc/"
#define MD_EXEC_PREFIX "/svr4/usr/lib/cmplrs/cc/"

/* Mips System V.4 doesn't have a getpagesize() function needed by the
   trampoline code, so use the POSIX sysconf function to get it.
   This is only done when compiling the trampoline code.  */

#ifdef  L_trampoline
#include <unistd.h>

/* In at least 5.0 and 5.01, there is no _SC_PAGE_SIZE macro, only a
   _SC_PAGESIZE macro.  */
#ifdef _SC_PAGESIZE
#define _SC_PAGE_SIZE _SC_PAGESIZE
#endif

#define getpagesize()	sysconf(_SC_PAGE_SIZE)
#endif /*  L_trampoline */

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

/* Generate calls to memcpy, etc., not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

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
