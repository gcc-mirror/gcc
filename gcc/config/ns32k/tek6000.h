/* Definitions of target machine for GNU compiler.
   Generic Tektronix 6000 series NS32000 version.
   See tek6100.h and tek6200.h, which include this file.
   Copyright (C) 1990 Free Software Foundation, Inc.
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

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

#include "ns32k.h"

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
#define PREFERRED_DEBUGGING_TYPE \
	((len > 1 && !strncmp(str, "go", len)) ? SDB_DEBUG : DBX_DEBUG )

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
  "-Dns16000 -Dns32000 -Dns32k -Dns32016 -DUTek -DUTEK -Dbsd -DBSD"
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "CPP_PREDEFINES_Tek6000"

/* This is how to align the code that follows an unconditional branch.
   Don't define it, since it confuses the assembler (we hear).  */

#undef ASM_OUTPUT_ALIGN_CODE

/* Assembler pseudo-op for shared data segment. */
#define SHARED_SECTION_ASM_OP ".shdata"

#ifdef UTEK_ASM
#undef  FUNCTION_PROLOGUE

/* This differs from the one in ns32k.h in printing a bitmask
   rather than a register list in the enter or save instruction.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno, g_regs_used = 0;				\
  int used_regs_buf[8], *bufp = used_regs_buf;			\
  int used_fregs_buf[8], *fbufp = used_fregs_buf;		\
  extern char call_used_regs[];					\
  MAIN_FUNCTION_PROLOGUE;					\
  for (regno = 0; regno < 8; regno++)				\
    if (regs_ever_live[regno]					\
	&& ! call_used_regs[regno])				\
    {								\
      *bufp++ = regno; g_regs_used++;				\
    }								\
  *bufp = -1;							\
  for (; regno < 16; regno++)					\
    if (regs_ever_live[regno] && !call_used_regs[regno]) {	\
      *fbufp++ = regno;						\
    }								\
  *fbufp = -1;							\
  bufp = used_regs_buf;						\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\tenter ");					\
  else if (g_regs_used)						\
    fprintf (FILE, "\tsave ");					\
  if (frame_pointer_needed || g_regs_used)			\
    {								\
      char mask = 0;						\
      while (*bufp >= 0)					\
	mask |= 1 << *bufp++;					\
      fprintf (FILE, "$0x%x", (int) mask & 0xff);		\
    }								\
  if (frame_pointer_needed)					\
    fprintf (FILE, ",$%d\n", SIZE);				\
  else if (g_regs_used)						\
    fprintf (FILE, "\n");					\
  fbufp = used_fregs_buf;					\
  while (*fbufp >= 0)						\
    {								\
      if ((*fbufp & 1) || (fbufp[0] != fbufp[1] - 1))		\
	fprintf (FILE, "\tmovf f%d,tos\n", *fbufp++ - 8);	\
      else							\
	{							\
	  fprintf (FILE, "\tmovl f%d,tos\n", fbufp[0] - 8);	\
	  fbufp += 2;						\
	}							\
    }								\
}

#undef  FUNCTION_EPILOGUE

/* This differs from the one in ns32k.h in printing a bitmask
   rather than a register list in the exit or restore instruction.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) \
{ register int regno, g_regs_used = 0, f_regs_used = 0;		\
  int used_regs_buf[8], *bufp = used_regs_buf;			\
  int used_fregs_buf[8], *fbufp = used_fregs_buf;		\
  extern char call_used_regs[];					\
  *fbufp++ = -2;						\
  for (regno = 8; regno < 16; regno++)				\
    if (regs_ever_live[regno] && !call_used_regs[regno]) {	\
       *fbufp++ = regno; f_regs_used++;				\
    }								\
  fbufp--;							\
  for (regno = 0; regno < 8; regno++)				\
    if (regs_ever_live[regno]					\
	&& ! call_used_regs[regno])				\
    {                                                         	\
      *bufp++ = regno; g_regs_used++;				\
    }                                                         	\
  while (fbufp > used_fregs_buf)				\
    {								\
      if ((*fbufp & 1) && fbufp[0] == fbufp[-1] + 1)		\
	{							\
	  fprintf (FILE, "\tmovl tos,f%d\n", fbufp[-1] - 8);	\
	  fbufp -= 2;						\
	}							\
      else fprintf (FILE, "\tmovf tos,f%d\n", *fbufp-- - 8);	\
    }								\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\texit ");					\
  else if (g_regs_used)						\
    fprintf (FILE, "\trestore ");				\
  if (g_regs_used || frame_pointer_needed)			\
    {								\
      char mask = 0;						\
								\
      while (bufp > used_regs_buf)				\
	{							\
	  /* Utek assembler takes care of reversing this */	\
	  mask |= 1 << *--bufp;					\
	}							\
      fprintf (FILE, "$0x%x\n", (int) mask & 0xff);		\
    }								\
  if (current_function_pops_args)				\
    fprintf (FILE, "\tret $%d\n", current_function_pops_args);	\
  else fprintf (FILE, "\tret $0\n"); }

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
