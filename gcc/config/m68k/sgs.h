/* Definitions of target machine for GNU compiler for m68k targets using
   assemblers derived from AT&T "SGS" releases.
   Copyright (C) 1991, 1993, 1996 Free Software Foundation, Inc.
   Written by Fred Fish (fnf@cygnus.com)

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

/* Control assembler-syntax conditionals in m68k.md and conditionals in
   m68k.h.  Note that some systems may also require SGS_SWAP_W and/or
   SGS_SWITCH_TABLES to be defined as well.  */

#define MOTOROLA		/* Use Motorola syntax rather than "MIT" */
#define SGS			/* Uses SGS assembler */
#define SGS_CMP_ORDER		/* Takes cmp operands in reverse order */

#include "m68k/m68k.h"

/* SGS specific assembler pseudo ops. */

#define	BYTE_ASM_OP		".byte"
#define WORD_ASM_OP		".short"
#define LONG_ASM_OP		".long"
#define SPACE_ASM_OP		".space"
#define ALIGN_ASM_OP		".align"
#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP		".global"
#define SWBEG_ASM_OP		".swbeg"
#define SET_ASM_OP		".set"

#define UNALIGNED_SHORT_ASM_OP	".short"	/* Used in dwarfout.c */
#define UNALIGNED_INT_ASM_OP	".long"		/* Used in dwarfout.c */

#define ASM_PN_FORMAT		"%s_%d"		/* Format for private names */

/* Here are four prefixes that are used by asm_fprintf to
   facilitate customization for alternate assembler syntaxes.
   Machines with no likelihood of an alternate syntax need not
   define these and need not use asm_fprintf.  */

/* The prefix for register names.  Note that REGISTER_NAMES
   is supposed to include this prefix. Also note that this is NOT an
   fprintf format string, it is a literal string */

#undef REGISTER_PREFIX
#define REGISTER_PREFIX "%"

/* The prefix for local (compiler generated) labels.
   These labels will not appear in the symbol table. */

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

/* The prefix to add to user-visible assembler symbols. */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* The prefix for immediate operands.  */

#undef IMMEDIATE_PREFIX
#define IMMEDIATE_PREFIX "&"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number.
   Motorola format uses different register names than defined in m68k.h.
   We also take this chance to convert 'a6' to 'fp' */

#undef REGISTER_NAMES

#ifndef SUPPORT_SUN_FPA

#define REGISTER_NAMES \
{"%d0",   "%d1",   "%d2",   "%d3",   "%d4",   "%d5",   "%d6",   "%d7",	     \
 "%a0",   "%a1",   "%a2",   "%a3",   "%a4",   "%a5",   "%fp",   "%sp",	     \
 "%fp0",  "%fp1",  "%fp2",  "%fp3",  "%fp4",  "%fp5",  "%fp6",  "%fp7" }

#else /* SUPPORTED_SUN_FPA */

#define REGISTER_NAMES \
{"%d0",   "%d1",   "%d2",   "%d3",   "%d4",   "%d5",   "%d6",   "%d7",	     \
 "%a0",   "%a1",   "%a2",   "%a3",   "%a4",   "%a5",   "%fp",   "%sp",	     \
 "%fp0",  "%fp1",  "%fp2",  "%fp3",  "%fp4",  "%fp5",  "%fp6",  "%fp7",	     \
 "%fpa0", "%fpa1", "%fpa2", "%fpa3", "%fpa4", "%fpa5", "%fpa6","%fpa7",	     \
 "%fpa8", "%fpa9", "%fpa10","%fpa11","%fpa12","%fpa13","%fpa14","%fpa15",    \
 "%fpa16","%fpa17","%fpa18","%fpa19","%fpa20","%fpa21","%fpa22","%fpa23",    \
 "%fpa24","%fpa25","%fpa26","%fpa27","%fpa28","%fpa29","%fpa30","%fpa31" }

#endif /* defined SUPPORT_SUN_FPA */

/* When using an SGS assembler, modify the name of the artificial label which
   identifies this file as having been compiled with gcc, and the macro that
   emits such a label in the assembly output, to use '%' rather than '.' */

#define ASM_IDENTIFY_GCC(FILE)				\
 { fprintf ((FILE), "%s:\n", "gcc2_compiled%"); }

/* This is how to output an assembler line defining an `int' constant.  */
/* The SGS assembler doesn't understand ".word". */

#undef ASM_OUTPUT_SHORT
#define ASM_OUTPUT_SHORT(FILE,VALUE)			\
( fprintf ((FILE), "\t%s ", WORD_ASM_OP),		\
  output_addr_const ((FILE), (VALUE)),			\
  fprintf ((FILE), "\n"))

#undef ASM_OUTPUT_LONG_DOUBLE
#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE)  			\
do { long l[3];							\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, l);		\
     fprintf ((FILE), "\t%s 0x%x,0x%x,0x%x\n", LONG_ASM_OP,	\
	     l[0], l[1], l[2]);					\
   } while (0)

/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)			\
do { long l[2];						\
     REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);		\
     fprintf ((FILE), "\t%s 0x%x,0x%x\n", LONG_ASM_OP,	\
	      l[0], l[1]);				\
   } while (0)

/* This is how to output an assembler line defining a `float' constant.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)			\
do { long l;						\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
     fprintf ((FILE), "\t%s 0x%x\n", LONG_ASM_OP, l);	\
   } while (0)

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)				\
  if ((LOG) > 0)						\
    fprintf ((FILE), "\t%s \t%u\n", ALIGN_ASM_OP, 1 << (LOG));	\
  else if ((LOG) > 31)						\
    abort ();

/* The routine used to output null terminated string literals.  We cannot
   use the ".string" pseudo op, because it silently truncates strings to
   1023 bytes.  There is no "partial string op" which works like ".string"
   but doesn't append a null byte, so we can't chop the input string up
   into small pieces and use that.  Our only remaining alternative is to
   output the string one byte at a time. */

#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)				\
do {								\
  register int sp = 0, lp = 0, ch;				\
  fprintf ((FILE), "\t%s ", BYTE_ASM_OP);			\
  do {								\
    ch = (PTR)[sp];						\
    if (ch > ' ' && ! (ch & 0x80) && ch != '\\')		\
      {								\
	fprintf ((FILE), "'%c", ch);				\
      }								\
    else							\
      {								\
	fprintf ((FILE), "0x%x", ch);				\
      }								\
    if (++sp < (LEN))						\
      {								\
	if ((sp % 10) == 0)					\
	  {							\
	    fprintf ((FILE), "\n\t%s ", BYTE_ASM_OP);		\
	  }							\
	else							\
	  {							\
	    putc (',', (FILE));					\
	  }							\
      }								\
  } while (sp < (LEN));						\
  putc ('\n', (FILE));						\
} while (0)


/* SGS based assemblers don't understand #NO_APP and #APP, so just don't
   bother emitting them. */

#undef ASM_APP_ON
#define ASM_APP_ON ""

#undef ASM_APP_OFF
#define ASM_APP_OFF ""

/* When using SGS derived assemblers, change the "MIT" or "MOTOROLA"
   to "SGS/AT&T"  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (68k, SGS/AT&T syntax)");

/* Use proper assembler syntax for these macros.  */
#undef ASM_OUTPUT_REG_PUSH
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  asm_fprintf (FILE, "\t%Omove.l %s,-(%Rsp)\n", reg_names[REGNO])

#undef ASM_OUTPUT_REG_POP
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  asm_fprintf (FILE, "\t%Omove.l (%Rsp)+,%s\n", reg_names[REGNO])

#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(CODE,FILE,VALUE)	\
  do { long l;						\
       REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
       asm_fprintf ((FILE), "%I0x%x", l);		\
     } while (0)
  
#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)		\
  do { long l[2];					\
       REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);		\
       asm_fprintf ((FILE), "%I0x%x%08x", l[0], l[1]);	\
     } while (0)

/* How to output a block of SIZE zero bytes.  Note that the `space' pseudo,
   when used in the text segment, causes SGS assemblers to output nop insns
   rather than 0s, so we set ASM_NO_SKIP_IN_TEXT to prevent this. */

#define ASM_NO_SKIP_IN_TEXT 1

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t%s %u\n", SPACE_ASM_OP, (SIZE))

/* Translate Motorola opcodes such as `jbeq' into SGS opcodes such
   as `beq.w'.
   Delete the `e' in `move...' and `fmove'.
   Change `ftst' to `ftest'.
   Change `fbne' to `fbneq'
   Change `fsne' to `fsneq'
   Change `divsl' to `tdivs' (32/32 -> 32r:32q)
   Change `divul' to `tdivu' (32/32 -> 32r:32q)
   Optionally change swap to swap.w.
   */

#ifdef SGS_SWAP_W
#define ASM_OUTPUT_OPCODE(FILE, PTR)			\
{							\
  extern int flag_pic;					\
  if (!strncmp ((PTR), "jbsr", 4))			\
    { if (flag_pic)					\
        fprintf ((FILE), "bsr");			\
      else						\
        fprintf ((FILE), "jsr");			\
    (PTR) += 4; }					\
  else if ((PTR)[0] == 'j' && (PTR)[1] == 'b')		\
    { ++(PTR);						\
      while (*(PTR) != ' ')				\
	{ putc (*(PTR), (FILE)); ++(PTR); }		\
      fprintf ((FILE), ".w"); }				\
  else if ((PTR)[0] == 's')				\
    {							\
      if (!strncmp ((PTR), "swap", 4))			\
	{ fprintf ((FILE), "swap.w"); (PTR) += 4; }	\
    }							\
/* FMOVE ==> FMOV, (and F%& F%$ translations) */	\
  else if ((PTR)[0] == 'f')				\
    {							\
      if (!strncmp ((PTR), "fmove", 5))			\
	{ fprintf ((FILE), "fmov"); (PTR) += 5; }	\
      else if (!strncmp ((PTR), "ftst", 4))		\
	{ fprintf ((FILE), "ftest"); (PTR) += 4; }	\
      else if (!strncmp ((PTR), "fbne", 4))		\
	{ fprintf ((FILE), "fbneq"); (PTR) += 4; }	\
      else if (!strncmp ((PTR), "fsne", 4))		\
	{ fprintf ((FILE), "fsneq"); (PTR) += 4; }	\
      else if (!strncmp ((PTR), "f%$move", 7))		\
	{ (PTR) += 7;					\
	  if (TARGET_68040_ONLY)			\
	    fprintf ((FILE), "fsmov");			\
	  else fprintf ((FILE), "fmov"); }		\
      else if (!strncmp ((PTR), "f%&move", 7))		\
	{ (PTR) += 7;					\
	  if (TARGET_68040_ONLY)			\
	    fprintf ((FILE), "fdmov");			\
	  else fprintf ((FILE), "fmov"); }		\
    }							\
/* MOVE, MOVEA, MOVEQ, MOVEC ==> MOV	*/		\
  else if ((PTR)[0] == 'm' && (PTR)[1] == 'o'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 'e')	\
    { fprintf ((FILE), "mov"); (PTR) += 4;		\
       if ((PTR)[0] == 'q' || (PTR)[0] == 'a'		\
	   || (PTR)[0] == 'c') (PTR)++; }		\
/* SUB, SUBQ, SUBA, SUBI ==> SUB */			\
  else if ((PTR)[0] == 's' && (PTR)[1] == 'u' 		\
	   && (PTR)[2] == 'b')				\
    { fprintf ((FILE), "sub"); (PTR) += 3;		\
       if ((PTR)[0] == 'q' || (PTR)[0] == 'i'	 	\
	   || (PTR)[0] == 'a') (PTR)++; }		\
/* CMP, CMPA, CMPI, CMPM ==> CMP	*/		\
  else if ((PTR)[0] == 'c' && (PTR)[1] == 'm'		\
	   && (PTR)[2] == 'p')				\
    { fprintf ((FILE), "cmp"); (PTR) += 3;		\
       if ((PTR)[0] == 'a' || (PTR)[0] == 'i'	 	\
	   || (PTR)[0] == 'm') (PTR)++; }		\
/* DIVSL ==> TDIVS */					\
  else if ((PTR)[0] == 'd' && (PTR)[1] == 'i'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 's'	\
	   && (PTR)[4] == 'l')				\
    { fprintf ((FILE), "tdivs"); (PTR) += 5; }		\
/* DIVUL ==> TDIVU */					\
  else if ((PTR)[0] == 'd' && (PTR)[1] == 'i'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 'u'	\
	   && (PTR)[4] == 'l')				\
    { fprintf ((FILE), "tdivu"); (PTR) += 5; }		\
}

#else /* not SGS_SWAP_W */

#define ASM_OUTPUT_OPCODE(FILE, PTR)			\
{							\
  extern int flag_pic;					\
  if (!strncmp ((PTR), "jbsr", 4))			\
    { if (flag_pic)					\
        fprintf ((FILE), "bsr");			\
      else						\
        fprintf ((FILE), "jsr");			\
    (PTR) += 4; }					\
  else if ((PTR)[0] == 'j' && (PTR)[1] == 'b')		\
    { ++(PTR);						\
      while (*(PTR) != ' ')				\
	{ putc (*(PTR), (FILE)); ++(PTR); }		\
      fprintf ((FILE), ".w"); }				\
/* FMOVE ==> FMOV, (and F%& F%$ translations) */	\
  else if ((PTR)[0] == 'f')				\
    {							\
      if (!strncmp ((PTR), "fmove", 5))			\
	{ fprintf ((FILE), "fmov"); (PTR) += 5; }	\
      else if (!strncmp ((PTR), "ftst", 4))		\
	{ fprintf ((FILE), "ftest"); (PTR) += 4; }	\
      else if (!strncmp ((PTR), "fbne", 4))		\
	{ fprintf ((FILE), "fbneq"); (PTR) += 4; }	\
      else if (!strncmp ((PTR), "fsne", 4))		\
	{ fprintf ((FILE), "fsneq"); (PTR) += 4; }	\
      else if (!strncmp ((PTR), "f%$move", 7))		\
	{ (PTR) += 7;					\
	  if (TARGET_68040_ONLY)			\
	    fprintf ((FILE), "fsmov");			\
	  else fprintf ((FILE), "fmov"); }		\
      else if (!strncmp ((PTR), "f%&move", 7))		\
	{ (PTR) += 7;					\
	  if (TARGET_68040_ONLY)			\
	    fprintf ((FILE), "fdmov");			\
	  else fprintf ((FILE), "fmov"); }		\
    }							\
/* MOVE, MOVEA, MOVEQ, MOVEC ==> MOV	*/		\
  else if ((PTR)[0] == 'm' && (PTR)[1] == 'o'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 'e')	\
    { fprintf ((FILE), "mov"); (PTR) += 4;		\
       if ((PTR)[0] == 'q' || (PTR)[0] == 'a'		\
	   || (PTR)[0] == 'c') (PTR)++; }		\
/* SUB, SUBQ, SUBA, SUBI ==> SUB */			\
  else if ((PTR)[0] == 's' && (PTR)[1] == 'u' 		\
	   && (PTR)[2] == 'b')				\
    { fprintf ((FILE), "sub"); (PTR) += 3;		\
       if ((PTR)[0] == 'q' || (PTR)[0] == 'i'	 	\
	   || (PTR)[0] == 'a') (PTR)++; }		\
/* CMP, CMPA, CMPI, CMPM ==> CMP	*/		\
  else if ((PTR)[0] == 'c' && (PTR)[1] == 'm'		\
	   && (PTR)[2] == 'p')				\
    { fprintf ((FILE), "cmp"); (PTR) += 3;		\
       if ((PTR)[0] == 'a' || (PTR)[0] == 'i'	 	\
	   || (PTR)[0] == 'm') (PTR)++; }		\
/* DIVSL ==> TDIVS */					\
  else if ((PTR)[0] == 'd' && (PTR)[1] == 'i'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 's'	\
	   && (PTR)[4] == 'l')				\
    { fprintf ((FILE), "tdivs"); (PTR) += 5; }		\
/* DIVUL ==> TDIVU */					\
  else if ((PTR)[0] == 'd' && (PTR)[1] == 'i'		\
	   && (PTR)[2] == 'v' && (PTR)[3] == 'u'	\
	   && (PTR)[4] == 'l')				\
    { fprintf ((FILE), "tdivu"); (PTR) += 5; }		\
}

#endif /* not SGS_SWAP_W */

/* This macro outputs the label at the start of a switch table.  The
   ".swbeg <N>" is an assembler directive that causes the switch table
   size to be inserted into the object code so that disassemblers, for
   example, can identify that it is the start of a switch table. */

#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE,PREFIX,NUM,TABLE)		\
  fprintf ((FILE), "\t%s &%d\n", SWBEG_ASM_OP, XVECLEN (PATTERN (TABLE), 1));

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLE)			\
  do {									\
    ASM_OUTPUT_BEFORE_CASE_LABEL((FILE),(PREFIX),(NUM),(TABLE));	\
    ASM_OUTPUT_INTERNAL_LABEL((FILE),(PREFIX),(NUM));			\
  } while (0)

/* At end of a switch table, define LDnnn iff the symbol LInnn was defined.
   Some SGS assemblers have a bug such that "Lnnn-LInnn-2.b(pc,d0.l*2)"
   fails to assemble.  Luckily "LDnnn(pc,d0.l*2)" produces the results
   we want.  This difference can be accommodated by making the assembler
   define such "LDnnn" to be either "Lnnn-LInnn-2.b", "Lnnn", or any other
   string, as necessary.  This is accomplished via the ASM_OUTPUT_CASE_END
   macro. */

#undef ASM_OUTPUT_CASE_END
#define ASM_OUTPUT_CASE_END(FILE,NUM,TABLE)		\
{ if (switch_table_difference_label_flag)		\
    asm_fprintf (FILE, "\t%s %LLD%d,%LL%d-%LLI%d-2.b\n",\
		 SET_ASM_OP, (NUM), (NUM), (NUM));	\
  switch_table_difference_label_flag = 0; }

extern int switch_table_difference_label_flag;

/* This is how to output an element of a case-vector that is relative.  */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)	\
  asm_fprintf (FILE, "\t%s %LL%d-%LL%d\n", WORD_ASM_OP, VALUE, REL)

/* Currently, JUMP_TABLES_IN_TEXT_SECTION must be defined in order to
   keep switch tables in the text section. */
   
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), ASM_PN_FORMAT, (NAME), (LABELNO)))

