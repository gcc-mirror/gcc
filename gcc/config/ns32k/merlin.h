/* Definitions of target machine for GNU compiler.  MERLIN NS32000 version.
   Copyright (C) 1990, 1994 Free Software Foundation, Inc.
   By Mark Mason (mason@reed.bitnet, pyramid!unify!mason@uunet.uu.net).

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

/* Two flags to control how addresses are printed in assembler insns.  */

#define SEQUENT_ADDRESS_BUG 1
#define SEQUENT_BASE_REGS

#include "ns32k/ns32k.h"

/* This is BSD, so it wants DBX format.  */
#define DBX_DEBUGGING_INFO

/* Sequent has some changes in the format of DBX symbols.  */
#define DBX_NO_XREFS 1

/* Don't split DBX symbols into continuations.  */
#define DBX_CONTIN_LENGTH 0

#define TARGET_DEFAULT 1

/* Print subsidiary information on the compiler version in use.  */
#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (32000, UTek syntax)");

/* These control the C++ compiler somehow.  */
#define FASCIST_ASSEMBLER
#define USE_COLLECT

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
	"-Dns32000 -Dns32k -Dns16000 -Dmerlin -Dunix -DUtek -Dbsd \
	-Asystem(unix) -Asystem(bsd) -Acpu(ns32k) -Amachine(ns32k)"

/* This is how to align the code that follows an unconditional branch.
   Don't define it, since it confuses the assembler (we hear).  */

#undef ASM_OUTPUT_ALIGN_CODE

/* Assembler pseudo-op for shared data segment. */
#define SHARED_SECTION_ASM_OP ".shdata"

/* %$ means print the prefix for an immediate operand. */

#ifdef UTEK_ASM
#undef PRINT_OPERAND
#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (CODE == '$') putc('$', FILE);					\
  else if (CODE == '?');						\
  else if (GET_CODE (X) == CONST_INT)					\
    fprintf(FILE, "$%d", INTVAL(X));					\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    {									\
      rtx xfoo;								\
      xfoo = XEXP (X, 0);						\
      switch (GET_CODE (xfoo))						\
	{								\
	case MEM:							\
	  if (GET_CODE (XEXP (xfoo, 0)) == REG)				\
	    if (REGNO (XEXP (xfoo, 0)) == STACK_POINTER_REGNUM)		\
	      fprintf (FILE, "0(0(sp))");				\
	    else fprintf (FILE, "0(0(%s))",				\
			  reg_names[REGNO (XEXP (xfoo, 0))]);		\
	  else								\
	    {								\
	      if (GET_CODE (XEXP (xfoo, 0)) == SYMBOL_REF		\
		  || GET_CODE (XEXP (xfoo, 0)) == CONST)		\
	        {							\
		  fprintf(FILE, "0(");					\
		  output_address(xfoo);					\
		  fprintf(FILE, "(sb))");				\
		}							\
	      else							\
	        {							\
		  fprintf (FILE, "0(");					\
		  output_address (xfoo);				\
		  putc (')', FILE);					\
		}							\
	    }								\
	  break;							\
	case REG:							\
	  fprintf (FILE, "0(%s)", reg_names[REGNO (xfoo)]);		\
	  break;							\
	case PRE_DEC:							\
	case POST_INC:							\
	  fprintf (FILE, "tos");					\
	  break;							\
	case CONST_INT:							\
	  fprintf (FILE, "$%d", INTVAL (xfoo));				\
	  break;							\
	default:							\
	  output_address (xfoo);					\
	  break;							\
	}								\
    }									\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != VOIDmode)	\
    if (GET_MODE (X) == DFmode)						\
      { union { double d; int i[2]; } u;				\
	u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
	fprintf (FILE, "$0d%.20e", u.d); }				\
    else { union { double d; int i[2]; } u;				\
	   u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X); \
	   fprintf (FILE, "$0f%.20e", u.d); }				\
  else output_addr_const (FILE, X); }

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
    fprintf (FILE, ",%d\n", SIZE);				\
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
    fprintf (FILE, "\tret %d\n", current_function_pops_args);	\
  else fprintf (FILE, "\tret 0\n"); }

#endif /* UTEK_ASM */

#undef PRINT_OPERAND_ADDRESS
#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address(FILE, ADDR)
