/* Definitions of target machine for GNU compiler.
   Generic Tektronix 6000 series NS32000 version.
   See ns32k/tek6100.h and ns32k/tek6200.h, which include this file.
   Copyright (C) 1990, 2000 Free Software Foundation, Inc.
   Created by Snoopy  (sopwith.uucp!snoopy).
   Based on work by Mark Mason (mason@reed.bitnet,
   pyramid!unify!mason@uunet.uu.net) and Keith Packard.

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

/* Generate syntax for the UTek assembler. */
#ifndef UTEK_ASM
#define UTEK_ASM
#endif

/* Two flags to control how addresses are printed in assembler insns.  */

/* The way PUT_ABSOLUTE_PREFIX in ns32k.h works, setting it to 0 will
 * turn it off.  Define ABSOLUTE_PREFIX before including ns32k.h.
 */
#define ABSOLUTE_PREFIX		0
#define IMMEDIATE_PREFIX	'$'

#include "ns32k/ns32k.h"

/* Define these after ns32k.c so we will notice if gcc tries to
 * output external mode addressing.  UTek's as and ld do not support
 * external mode addressing, according to Daryl McDaniel (illian.uucp!darylm).
 * Hopefully the UTek assembler will complain if gcc feeds it this stuff.
 * They don't seem to do anything, I think that gcc is not actually
 * trying to generate external mode operands.
 */
#undef PUT_EXTERNAL_PREFIX
#define PUT_EXTERNAL_PREFIX(arg)  fprintf(arg, "  Should not be using external mode under UTek.  ")
#define EXTERNAL_PREFIX '%'

/* Used in ns32k.c to control syntax. */
#define NO_ABSOLUTE_PREFIX_IF_SYMBOLIC
#define NO_IMMEDIATE_PREFIX_IF_SYMBOLIC

/* Used in ns32k.md to specify syntax of bsr/jsr operand. */
#define CALL_MEMREF_IMPLICIT

/* #define PC_RELATIVE */	/* Seems to break things. */
#define BASE_REG_NEEDED		/* Seems to fix problem where external mode
				 * syntax was being generated.
				 */

/*  ------------   Debugging Support   ----------------------------- */

/* The sdb support does not yet work with UTek.  Need to teach gcc
 * how to create sdb type stabs as well as dbx style stabs.
 */
#define DBX_DEBUGGING_INFO
/* #define SDB_DEBUGGING_INFO */

/* Act the same as the UTek complier: -g for dbx, -go for sdb.
 * This is used in toplev.c.
 */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define CC1_SPEC "{go:-gcoff}"
#define CC1PLUS_SPEC "{go:-gcoff}"

/* Sequent has some changes in the format of DBX symbols.  */
#define DBX_NO_XREFS 1

/* Don't split DBX symbols into continuations.  */
#define DBX_CONTIN_LENGTH 0

/* ------------------------------------------- */

#define TARGET_DEFAULT 1

/* These control the C++ compiler somehow.  */
#define FASCIST_ASSEMBLER
#define USE_COLLECT

/* Print subsidiary information on the compiler version in use.  */
#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (ns32k, UTek syntax)");

/* The tek6100.h and tek6200.h files add stratos or merlin respectively. */

#define CPP_PREDEFINES_Tek6000 \
  "-Dns16000 -Dns32000 -Dns32k -Dns32016 -DUTek -DUTEK -Dbsd -DBSD \
   -Asystem=unix -Asystem=bsd -Acpu=ns32k -Amachine=ns32k"
#undef CPP_PREDEFINES
#define CPP_PREDEFINES CPP_PREDEFINES_Tek6000

/* This is how to align the code that follows an unconditional branch.
   Don't define it, since it confuses the assembler (we hear).  */

#undef LABEL_ALIGN_AFTER_BARRIER

/* Assembler pseudo-op for shared data segment. */
#define SHARED_SECTION_ASM_OP "\t.shdata"

#ifdef UTEK_ASM

/* UTek assembler needs "ret $0", not "ret 0". */
#undef  TRANSFER_FROM_TRAMPOLINE
#define TRANSFER_FROM_TRAMPOLINE	\
void					\
__transfer_from_trampoline ()		\
{					\
  asm ("___trampoline:");		\
  asm ("movd 16(r2),tos");		\
  asm ("movd 12(r2),r2");		\
  asm ("ret $0");			\
}

#endif /* UTEK_ASM */

#undef PRINT_OPERAND_ADDRESS
#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address(FILE, ADDR)

/* The UTek library supplies bcopy() and friends, not memcpy(). */
#ifdef TARGET_MEM_FUNCTIONS
#undef TARGET_MEM_FUNCTIONS
#endif
