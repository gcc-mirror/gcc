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

/* Two flags to control how addresses are printed in assembler insns.  */
#define SEQUENT_ADDRESS_BUG 1
#define SEQUENT_BASE_REGS

#include "ns32k.h"

/* This is BSD, so it wants DBX format.  */
#define DBX_DEBUGGING_INFO

/* Sequent has some changes in the format of DBX symbols.  */
#define DBX_NO_XREFS 1

/* Don't split DBX symbols into continuations.  */
#define DBX_CONTIN_LENGTH 0

#define TARGET_DEFAULT 9  /* 32332 with 32081 (guessing) */

/* Print subsidiary information on the compiler version in use.  */
#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (32000, Sequent syntax)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dns32000 -Dsequent -Dunix"

/* Link with libg.a when debugging, for dbx's sake.  */

#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} "

/* This is how to align the code that follows an unconditional branch.
   Don't define it, since it confuses the assembler (we hear).  */

#undef ASM_OUTPUT_ALIGN_CODE

/* Assember pseudo-op for shared data segment. */
#define SHARED_SECTION_ASM_OP ".shdata"

/* Control how stack adjust insns are output.  */
#define SEQUENT_ADJUST_STACK

/* %$ means print the prefix for an immediate operand.
   On the sequent, no prefix is used for such.  */

#undef PRINT_OPERAND
#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (CODE == '$') ;							\
  else if (CODE == '?');						\
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
	      fprintf (FILE, "0(");					\
	      output_address (xfoo);					\
	      putc (')', FILE);						\
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
	  fprintf (FILE, "@%d", INTVAL (xfoo));				\
	  break;							\
	default:							\
	  output_address (xfoo);					\
	  break;							\
	}								\
    }									\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != DImode)	\
    if (GET_MODE (X) == DFmode)						\
      { union { double d; int i[2]; } u;				\
	u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
	fprintf (FILE, "0d%.20e", u.d); }				\
    else { union { double d; int i[2]; } u;				\
	   u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X); \
	   fprintf (FILE, "0f%.20e", u.d); }				\
  else output_addr_const (FILE, X); }

#undef PRINT_OPERAND_ADDRESS
#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address(FILE, ADDR)
