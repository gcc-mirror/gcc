/* Definitions of target machine for GNU compiler.
   Commodore Amiga A3000UX version.

   Copyright (C) 1991, 1993 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "m68k/m68kv4.h"

/* rhealey@aggregate.com says dots are no good either.  */
#define NO_DOT_IN_LABEL

/* Alter assembler syntax for fsgldiv and fsglmul.
   It is highly likely that this is a generic SGS m68k assembler dependency.
   If so, it should eventually be handled in the m68k/sgs.h ASM_OUTPUT_OPCODE
   macro, like the other SGS assembler quirks.  -fnf */

#define FSGLDIV_USE_S		/* Use fsgldiv.s, not fsgldiv.x */
#define FSGLMUL_USE_S		/* Use fsglmul.s, not fsglmul.x */

/* Names to predefine in the preprocessor for this target machine.  For the
   Amiga, these definitions match those of the native AT&T compiler.  Note
   that we override the definition in m68kv4.h, where SVR4 is defined and
   AMIX isn't. */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-Dm68k -Dunix -DAMIX -D__svr4__ -D__motorola__ \
 -Amachine(m68k) -Acpu(m68k) -Asystem(unix) -Alint(off)"

/* At end of a switch table, define LDnnn iff the symbol LInnn was defined.
   Some SGS assemblers have a bug such that "Lnnn-LInnn-2.b(pc,d0.l*2)"
   fails to assemble.  Luckily "Lnnn(pc,d0.l*2)" produces the results
   we want.  This difference can be accommodated by making the assembler
   define such "LDnnn" to be either "Lnnn-LInnn-2.b", "Lnnn", or any other
   string, as necessary.  This is accomplished via the ASM_OUTPUT_CASE_END
   macro. (the Amiga assembler has this bug) */

#undef ASM_OUTPUT_CASE_END
#define ASM_OUTPUT_CASE_END(FILE,NUM,TABLE)				\
do {									\
  if (switch_table_difference_label_flag)				\
    asm_fprintf ((FILE), "\t%s %LLD%d,%LL%d\n", SET_ASM_OP, (NUM), (NUM));\
  switch_table_difference_label_flag = 0;				\
} while (0)

int switch_table_difference_label_flag;

/* This says how to output assembler code to declare an
   uninitialized external linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.
   [This macro overrides the one in svr4.h because the amix assembler
    has a minimum default alignment of 4, and will not accept any
    explicit alignment smaller than this.  -fnf] */

#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fputs ("\t.comm\t", (FILE));						\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), ",%u,%u\n", (SIZE), MAX ((ALIGN) / BITS_PER_UNIT, 4)); \
} while (0)

/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.
   [This macro overrides the one in svr4.h because the amix assembler
    has a minimum default alignment of 4, and will not accept any
    explicit alignment smaller than this.  -fnf] */

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "\t%s\t%s,%u,%u\n",					\
	   BSS_ASM_OP, (NAME), (SIZE), MAX ((ALIGN) / BITS_PER_UNIT, 4)); \
} while (0)

/* This definition of ASM_OUTPUT_ASCII is the same as the one in m68k/sgs.h,
   which has been overridden by the one in svr4.h.  However, we can't use
   the one in svr4.h because the amix assembler croaks on some of the
   strings that it emits (such as .string "\"%s\"\n"). */

#undef ASM_OUTPUT_ASCII
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

/* The following should be unnecessary as a result of PIC_CASE_VECTOR_ADDRESS.
   But rhealey@aggregate.com says they are still needed.  */

/* Override these for the sake of an assembler bug: the Amix
   assembler can't handle .LC0@GOT syntax.  This pollutes the final
   table for shared librarys but what's a poor soul to do; sigh... RFH */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  if (flag_pic && !strcmp(PREFIX,"LC"))			\
    sprintf (LABEL, "*%s%%%d", PREFIX, NUM);		\
  else							\
    sprintf (LABEL, "*%s%s%d", LOCAL_LABEL_PREFIX, PREFIX, NUM)

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  if (flag_pic && !strcmp(PREFIX,"LC"))			\
    asm_fprintf (FILE, "%s%%%d:\n", PREFIX, NUM);	\
  else							\
    asm_fprintf (FILE, "%0L%s%d:\n", PREFIX, NUM)
