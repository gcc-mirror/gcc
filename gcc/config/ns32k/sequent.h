/* Definitions of target machine for GNU compiler.  SEQUENT NS32000 version.
   Copyright (C) 1987 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@mcc.com)

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

#include "ns32k.h"

/* This is BSD, so it wants DBX format.  */
#define DBX_DEBUGGING_INFO

/* Sequent has some changes in the format of DBX symbols.  */
#define DBX_NO_XREFS 1

/* Don't split DBX symbols into continuations.  */
#define DBX_CONTIN_LENGTH 0

#define TARGET_DEFAULT 9  /* 32332 with 32081 (guessing).  */

/* Print subsidiary information on the compiler version in use.  */
#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (32000, Sequent syntax)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dns32000 -Dsequent -Dunix"

/* Link with libg.a when debugging, for dbx's sake.  */

#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} "

/* gcc should find libgcc.a itself, not ask linker to do so.  */

#define LINK_LIBGCC_SPECIAL

/* GCC must match what sys/types.h uses for size_t.  */

#define SIZE_TYPE "int"

/* This is how to align the code that follows an unconditional branch.
   Don't define it, since it confuses the assembler (we hear).  */

#undef ASM_OUTPUT_ALIGN_CODE

/* Assembler pseudo-op for shared data segment. */
#define SHARED_SECTION_ASM_OP ".shdata"

/* Control how stack adjust insns are output.  */
#define SEQUENT_ADJUST_STACK

#define NO_ABSOLUTE_PREFIX_IF_SYMBOLIC

#define IMMEDIATE_PREFIX 0

#define SEQUENT_ASM

/* Operand of bsr or jsr should be just the address.  */

#define CALL_MEMREF_IMPLICIT

/* Output a reg as an index rather than a base if we have the choice.  */

#define INDEX_RATHER_THAN_BASE
