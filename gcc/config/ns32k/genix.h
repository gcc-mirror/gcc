/* Definitions of target machine for GNU compiler.  Genix ns32000 version.
   Copyright (C) 1987, 1988, 1994 Free Software Foundation, Inc.

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

#include "ns32k/encore.h"

/* We don't want the one Encore needs.  */
#undef ASM_SPEC

/* The following defines override ones in ns32k.h and prevent any attempts
   to explicitly or implicitly make references to the SB register in the GCC
   generated code.  It is necessary to avoid such references under Genix V.3.1
   because this OS doesn't even save/restore the SB on context switches!  */

#define IS_OK_REG_FOR_BASE_P(X)						\
  ( (GET_CODE (X) == REG) && REG_OK_FOR_BASE_P (X) )

#undef INDIRECTABLE_1_ADDRESS_P
#define INDIRECTABLE_1_ADDRESS_P(X)					\
  (CONSTANT_ADDRESS_NO_LABEL_P (X)					\
   || IS_OK_REG_FOR_BASE_P (X)						\
   || (GET_CODE (X) == PLUS						\
       && IS_OK_REG_FOR_BASE_P (XEXP (X, 0))				\
       && CONSTANT_ADDRESS_P (XEXP (X, 1))  )  )

/* Note that for double indirects, only FP, SP, and SB are allowed
   as the inner-most base register.  But we are avoiding use of SB.  */

#undef MEM_REG
#define MEM_REG(X)							\
  ( (GET_CODE (X) == REG)						\
  && ( (REGNO (X) == FRAME_POINTER_REGNUM)				\
    || (REGNO (X) == STACK_POINTER_REGNUM) ) )

#undef INDIRECTABLE_2_ADDRESS_P
#define INDIRECTABLE_2_ADDRESS_P(X)					\
  (GET_CODE (X) == MEM							\
   && (((xfoo0 = XEXP (X, 0), MEM_REG (xfoo0))				\
       || (GET_CODE (xfoo0) == PLUS					\
	   && MEM_REG (XEXP (xfoo0, 0))					\
	   && CONSTANT_ADDRESS_NO_LABEL_P (XEXP (xfoo0, 1))))		\
       || CONSTANT_ADDRESS_NO_LABEL_P (xfoo0)))

/* Go to ADDR if X is a valid address not using indexing.
   (This much is the easy part.)  */
#undef GO_IF_NONINDEXED_ADDRESS
#define GO_IF_NONINDEXED_ADDRESS(X, ADDR)				\
{ register rtx xfoob = (X);						\
  if (GET_CODE (xfoob) == REG) goto ADDR;				\
  if (INDIRECTABLE_1_ADDRESS_P(X)) goto ADDR;				\
  if (CONSTANT_P(X)) goto ADDR;						\
  if (INDIRECTABLE_2_ADDRESS_P (X)) goto ADDR;				\
  if (GET_CODE (X) == PLUS)						\
    if (CONSTANT_ADDRESS_NO_LABEL_P (XEXP (X, 1)))			\
      if (INDIRECTABLE_2_ADDRESS_P (XEXP (X, 0)))			\
	goto ADDR;							\
}

/* A bug in the GNX 3.X assembler causes references to external symbols to
   be mishandled if the symbol is also used as the name of a function-local
   variable or as the name of a struct or union field.  The problem only
   appears when you are also using the -g option so that SDB debugging
   directives are also being produced by GCC.  In such cases, the assembler
   gets the external entity confused with the local entity and addressing
   havoc ensues.  The solution is to get GCC to produce .global directives
   for all external entities which are actually referenced within the current
   source file.  The following macro does this.  */

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)				\
    ASM_GLOBALIZE_LABEL(FILE,NAME);

/* Genix wants 0l instead of 0f.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)				\
 fprintf (FILE, "\t.long 0l%.20e\n", (VALUE))

/*  A bug in the GNX 3.X linker prevents symbol-table entries with a storage-
    class field of C_EFCN (-1) from being accepted. */

#ifdef PUT_SDB_EPILOGUE_END
#undef PUT_SDB_EPILOGUE_END
#endif
#define PUT_SDB_EPILOGUE_END(NAME)

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (32000, National syntax)");

/* Same as the encore definition except
   * Different syntax for double constants.
   * Don't output `?' before external regs.
   * Output `(sb)' in certain indirect refs.  */

#error this has not been updated since version 1.
#error it is certainly wrong.

#undef PRINT_OPERAND
#define PRINT_OPERAND(FILE, X, CODE)					\
{ if (CODE == '$') putc ('$', FILE);					\
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
	      extern int paren_base_reg_printed;			\
	      fprintf (FILE, "0(");					\
	      paren_base_reg_printed = 0;				\
	      output_address (xfoo);					\
	      if (!paren_base_reg_printed)				\
		fprintf (FILE, "(sb)");					\
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
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != VOIDmode)	\
    if (GET_MODE (X) == DFmode)						\
      { union { double d; int i[2]; } u;				\
        u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
	fprintf (FILE, "$0l%.20e", u.d); }				\
    else { union { double d; int i[2]; } u;				\
	   u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X); \
	   fprintf (FILE, "$0f%.20e", u.d); }				\
  else if (GET_CODE (X) == CONST)					\
    output_addr_const (FILE, X);					\
  else { putc ('$', FILE); output_addr_const (FILE, X); }}
