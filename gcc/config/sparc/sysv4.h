/* Target definitions for GNU compiler for Sparc running System V.4
   Copyright (C) 1991 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@ncd.com).

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

#include "sparc.h"
#include "svr4.h"

/* Provide a set of pre-definitions and pre-assertions appropriate for
   the sparc running svr4.  __svr4__ is our extension.  */

#define CPP_PREDEFINES \
  "-Dsparc -Dunix -D__svr4__ -Asystem(unix) -Acpu(sparc) -Amachine(sparc)"

#define ASM_COMMENT_START "!"

#undef TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT      "#%s"

/* Define how the sparc registers should be numbered for Dwarf output.
   The numbering provided here should be compatible with the native
   svr4 SDB debugger in the sparc/svr4 reference port.  */

#define DBX_REGISTER_NUMBER(REGNO)					\
  ((REGNO) < 32) ? (REGNO) : ((REGNO) + 8))

/* A pair of defines for the set of pseudo-ops used to switch to the
   .ctors and .dtors sections.  Note that on the sparc, all user-defined
   sections have the "progbits" attribute by default, so we don't even
   specify it here.  */

#define CTORS_SECTION_ASM_OP    "\t.section\t.ctors,\"a\""
#define DTORS_SECTION_ASM_OP    "\t.section\t.dtors,\"a\""
