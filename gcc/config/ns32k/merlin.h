/* Definitions of target machine for GNU compiler.  MERLIN NS32000 version.
   Copyright (C) 1990, 1994, 2000 Free Software Foundation, Inc.
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

#define MERLIN_TARGET

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
	-Asystem=unix -Asystem=bsd -Acpu=ns32k -Amachine=ns32k"

/* This is how to align the code that follows an unconditional branch.
   Don't define it, since it confuses the assembler (we hear).  */

#undef LABEL_ALIGN_AFTER_BARRIER

/* Assembler pseudo-op for shared data segment. */
#define SHARED_SECTION_ASM_OP "\t.shdata"

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

#endif /* UTEK_ASM */

#undef PRINT_OPERAND_ADDRESS
#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address(FILE, ADDR)
