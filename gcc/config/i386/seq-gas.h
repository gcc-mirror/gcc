/* Definitions for Sequent Intel 386 using GAS.
   Copyright (C) 1992 Free Software Foundation, Inc.

/* Mostly it's like a Sequent 386 without GAS. */

#include "i386/sequent.h"

/* A C statement or statements which output an assembler instruction
   opcode to the stdio stream STREAM.  The macro-operand PTR is a
   variable of type `char *' which points to the opcode name in its
   "internal" form--the form that is written in the machine description.

   GAS version 1.38.1 doesn't understand the `repz' opcode mnemonic.
   So use `repe' instead.  */

#undef ASM_OUTPUT_OPCODE
#define ASM_OUTPUT_OPCODE(STREAM, PTR)	\
{									\
  if ((PTR)[0] == 'r'							\
      && (PTR)[1] == 'e'						\
      && (PTR)[2] == 'p')						\
    {									\
      if ((PTR)[3] == 'z')						\
	{								\
	  fprintf (STREAM, "repe");					\
	  (PTR) += 4;							\
	}								\
      else if ((PTR)[3] == 'n' && (PTR)[4] == 'z')			\
	{								\
	  fprintf (STREAM, "repne");					\
	  (PTR) += 5;							\
	}								\
    }									\
}

/* Define macro used to output shift-double opcodes when the shift
   count is in %cl.  Some assemblers require %cl as an argument;
   some don't.

   GAS requires the %cl argument, so override i386/unix.h. */

#undef AS3_SHIFT_DOUBLE
#define AS3_SHIFT_DOUBLE(a,b,c,d) AS3 (a,b,c,d)

/* Print opcodes the way that GAS expects them. */
#define GAS_MNEMONICS 1
