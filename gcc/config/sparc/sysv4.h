/* Target definitions for GNU compiler for Sparc running System V.4
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@ncd.com).

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

#include "sparc.h"

/* Undefine some symbols which are defined in "sparc.h" but which are
   appropriate only for SunOS 4.x, and not for svr4.  */

#undef DBX_DEBUGGING_INFO
#undef WORD_SWITCH_TAKES_ARG
#undef SELECT_SECTION
#undef ASM_DECLARE_FUNCTION_NAME
#undef TEXT_SECTION_ASM_OP
#undef DATA_SECTION_ASM_OP

#include "svr4.h"

/* Undefined some symbols which are defined in "svr4.h" but which are
   appropriate only for typical svr4 systems, but not for the specific
   case of svr4 running on a Sparc.  */

#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#undef INIT_SECTION_ASM_OP
#undef CONST_SECTION_ASM_OP
#undef TYPE_OPERAND_FMT
#undef PUSHSECTION_FORMAT
#undef STRING_ASM_OP
#undef COMMON_ASM_OP
#undef SKIP_ASM_OP
#undef SET_ASM_OP	/* Has no equivalent.  See ASM_OUTPUT_DEF below.  */

/* Provide a set of pre-definitions and pre-assertions appropriate for
   the Sparc running svr4.  __svr4__ is our extension.  */

#define CPP_PREDEFINES \
  "-Dsparc -Dunix -D__svr4__ -Asystem(unix) -Acpu(sparc) -Amachine(sparc)"

/* The specialized code which needs to appear in the .init section prior
   to the prologue code for `__do_global_ctors' (see crtstuff.c).

   On Sparcs running svr4, the /usr/ccs/lib/crti.o file (with gets linked
   in prior to the crtbegin.o file) has a single `save' instruction in its
   .init section.  That `save' instruction tries to setup a stack frame for
   the sake of any subsequent code in the .init section.  Unfortunately,
   the size it uses for the stack frame is only a guess, and is not really
   adequate for our purposes.  More importantly, we independently put our
   own standard function prologue (for __do_global_ctors) into the .init
   section and that function prologue includes its own `save' instruction!
   Thus, unless we do something to correct the situation, we'll get *two*
   stack frames allocated when crt0.o calls the code in the .init section,
   and havoc will ensue.  The following macro definition prevents such woes.
*/

#define INIT_SECTION_PREAMBLE	asm ("restore")

/* This is the string used to begin an assembly language comment for the
   Sparc/svr4 assembler.  */

#define ASM_COMMENT_START "!"

/* Define the names of various pseudo-op used by the Sparc/svr4 assembler.
   Note that many of these are different from the typical pseudo-ops used
   by most svr4 assemblers.  That is probably due to a (misguided?) attempt
   to keep the Sparc/svr4 assembler somewhat compatible with the Sparc/SunOS
   assembler.  */

#define STRING_ASM_OP		".asciz"
#define COMMON_ASM_OP		".common"
#define SKIP_ASM_OP		".skip"
#define UNALIGNED_INT_ASM_OP	".uaword"
#define UNALIGNED_SHORT_ASM_OP	".uahalf"
#define PUSHSECTION_ASM_OP	".pushsection"
#define POPSECTION_ASM_OP	".popsection"

/* This is the format used to print the second operand of a .type pseudo-op
   for the Sparc/svr4 assembler.  */

#define TYPE_OPERAND_FMT      "#%s"

/* This is the format used to print a .pushsection pseudo-op (and its operand)
   for the Sparc/svr4 assembler.  */

#define PUSHSECTION_FORMAT	"\t%s\t\"%s\"\n"

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, JUMPTABLE)		\
do { ASM_OUTPUT_ALIGN ((FILE), 2);					\
     ASM_OUTPUT_INTERNAL_LABEL ((FILE), PREFIX, NUM);			\
   } while (0)

/* This is how to equate one symbol to another symbol.  The syntax used is
   `SYM1=SYM2'.  Note that this is different from the way equates are done
   with most svr4 assemblers, where the syntax is `.set SYM1,SYM2'.  */

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {	fprintf ((FILE), "\t");						\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, " = ");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)

/* Define how the Sparc registers should be numbered for Dwarf output.
   The numbering provided here should be compatible with the native
   svr4 SDB debugger in the Sparc/svr4 reference port.  The numbering
   is as follows:

   Assembly name	gcc internal regno	Dwarf regno
   ----------------------------------------------------------
   g0-g7		0-7			0-7
   o0-o7		8-15			8-15
   l0-l7		16-23			16-23
   i0-i7		24-31			24-31
   f0-f31		32-63			40-71
*/

#define DBX_REGISTER_NUMBER(REGNO)					\
  (((REGNO) < 32) ? (REGNO)						\
   : ((REGNO) < 63) ? ((REGNO) + 8)					\
   : (abort (), 0))

/* A set of symbol definitions for assembly pseudo-ops which will
   get us switched to various sections of interest.  These are used
   in all places where we simply want to switch to a section, and
   *not* to push the previous section name onto the assembler's
   section names stack (as we do often in dwarfout.c).  */

#define TEXT_SECTION_ASM_OP	".section\t\".text\""
#define DATA_SECTION_ASM_OP	".section\t\".data\""
#define BSS_SECTION_ASM_OP	".section\t\".bss\""
#define CONST_SECTION_ASM_OP	".section\t\".rodata\""
#define INIT_SECTION_ASM_OP	".section\t\".init\",#alloc"
#define CTORS_SECTION_ASM_OP    ".section\t\".ctors\",#alloc"
#define DTORS_SECTION_ASM_OP    ".section\t\".dtors\",#alloc"

/* If the host and target formats match, output the floats as hex.  */
#if HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
#if defined (HOST_WORDS_BIG_ENDIAN) == WORDS_BIG_ENDIAN

/* This is how to output assembly code to define a `float' constant.
   We always have to use a .long pseudo-op to do this because the native
   SVR4 ELF assembler is buggy and it generates incorrect values when we
   try to use the .float pseudo-op instead.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
do { long value;							\
     REAL_VALUE_TO_TARGET_SINGLE ((VALUE), value);			\
     fprintf((FILE), "\t.long\t0x%x\n", value);				\
   } while (0)

/* This is how to output assembly code to define a `double' constant.
   We always have to use a pair of .long pseudo-ops to do this because
   the native SVR4 ELF assembler is buggy and it generates incorrect
   values when we try to use the the .double pseudo-op instead.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
do { long value[2];							\
     REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), value);			\
     fprintf((FILE), "\t.long\t0x%x\n", value[0]);			\
     fprintf((FILE), "\t.long\t0x%x\n", value[1]);			\
   } while (0)

#endif /* word order matches */
#endif /* HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT */
