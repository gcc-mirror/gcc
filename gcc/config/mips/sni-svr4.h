/* Definitions of target machine for GNU compiler.  SNI SINIX version.
   Copyright (C) 1996, 1997, 1999, 2000, 2002 Free Software Foundation, Inc.
   Contributed by Marco Walther (Marco.Walther@mch.sni.de).

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

#define TARGET_OS_CPP_BUILTINS()			\
    do {						\
	builtin_define_std ("host_mips");		\
	builtin_define_std ("SYSTYPE_SVR4");		\
	builtin_define_std ("unix");			\
	builtin_define_std ("mips");			\
	builtin_define_std ("sinix");			\
	builtin_define_std ("SNI");			\
	builtin_assert ("system=unix");			\
	builtin_assert ("system=svr4");			\
	builtin_assert ("machine=mips");		\
} while (0)

#define LINK_SPEC "\
%{G*} \
%{!mgas: \
	%{dy} %{dn}}"

#define LIB_SPEC "\
	%{p:-lprof1} \
	%{!p:%{pg:-lprof1} \
	     %{!pg:-L/usr/ccs/lib/ -lc /usr/ccs/lib/crtn.o%s}}"

#define STARTFILE_SPEC "\
	%{pg:gcrt0.o%s} \
	%{!pg:%{p:mcrt0.o%s} \
	       %{!p:/usr/ccs/lib/crt1.o /usr/ccs/lib/crti.o /usr/ccs/lib/values-Xt.o%s}}"

/* Mips System V.4 doesn't have a getpagesize() function needed by the
   trampoline code, so use the POSIX sysconf function to get it.
   This is only done when compiling the trampoline code.  */

#ifdef  L_trampoline
#include <unistd.h>

#define getpagesize()	sysconf(_SC_PAGE_SIZE)
#endif /*  L_trampoline */

#define OBJECT_FORMAT_ELF

#define	TARGET_DEFAULT	MASK_ABICALLS
#define ABICALLS_ASM_OP "\t.option pic2"

#define MACHINE_TYPE "SNI running SINIX 5.42"

#define MIPS_DEFAULT_GVALUE	0

#define NM_FLAGS	"-p"

#define ASM_GLOBAL	".rdata\n\t\t.globl\t"

#include "mips/mips.h"

/* We do not want to run mips-tfile!  */
#undef ASM_FINAL_SPEC

#undef OBJECT_FORMAT_COFF

/* We don't support debugging info for now.  */
#undef DBX_DEBUGGING_INFO
#undef SDB_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE

#define DWARF2_UNWIND_INFO 0
