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
#undef DEF_ASM_OP	/* Has no equivalent.  See ASM_OUTPUT_DEF below.  */
#undef ASM_GENERATE_INTERNAL_LABEL
#undef ASM_OUTPUT_INTERNAL_LABEL

/* Provide a set of pre-definitions and pre-assertions appropriate for
   the Sparc running svr4.  __svr4__ is our extension.  */

#define CPP_PREDEFINES \
  "-Dsparc -Dunix -D__svr4__ -Asystem(unix) -Acpu(sparc) -Amachine(sparc)"

/* This is the string used to begin an assembly language comment for the
   Sparc/svr4 assembler.  */

#define ASM_COMMENT_START "!"

/* Define the names of various pseudo-op used by the Sparc/svr4 assembler.
   Note that many of these are different from the typical pseudo-ops used
   by most svr4 assemblers.  That is probably due to a (misguided?) attempt
   to keep the Sparc/svr4 assembler somewhat compatible with the Sparc/SunOS
   assembler.  */

#define STRING_ASM_OP		"\t.asciz"
#define COMMON_ASM_OP		"\t.common"
#define SKIP_ASM_OP		"\t.skip"
#define UNALIGNED_INT_ASM_OP	"\t.uaword"
#define UNALIGNED_SHORT_ASM_OP	"\t.uahalf"
#define PUSHSECTION_ASM_OP	"\t.pushsection"
#define POPSECTION_ASM_OP	"\t.popsection"

/* This is the format used to print the second operand of a .type pseudo-op
   for the Sparc/svr4 assembler.  */

#define TYPE_OPERAND_FMT      "#%s"

/* This is the format used to print a .pushsection pseudo-op (and its operand)
   for the Sparc/svr4 assembler.  */

#define PUSHSECTION_FORMAT	"%s\t\"%s\"\n"

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

/* Generate the special assembly code needed to align the start of a jump
   tables.  Under svr4, jump tables go into the .rodata section.  Other
   things (e.g. constants) may be put into the .rodata section too, and
   those other things may end on odd (i.e. unaligned) boundaries, so we
   need to get re-aligned just before we output each jump table.  */

#define ASM_OUTPUT_ALIGN_JUMP_TABLE(FILE) ASM_OUTPUT_ALIGN ((FILE), 2)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.

   If the NUM argument is negative, we don't use it when generating the
   label.

   For most svr4 systems, the convention is that any symbol which begins
   with a period is not put into the linker symbol table by the assembler,
   however the current Sparc/svr4 assembler is brain-dammaged and it needs
   to see `.L' at the start of a symbol or else it will be put into the
   linker symbol table.
*/

#define ASM_OUTPUT_INTERNAL_LABEL(FILE, PREFIX, NUM)			\
do {									\
  if ((int) (NUM) >= 0)							\
    fprintf (FILE, ".L%s%d:\n", PREFIX, NUM);				\
  else									\
    fprintf (FILE, ".L%s:\n", PREFIX);					\
} while (0)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.

   If the NUM argument is negative, we don't use it when generating the
   label.

   For most svr4 systems, the convention is that any symbol which begins
   with a period is not put into the linker symbol table by the assembler,
   however the current Sparc/svr4 assembler is brain-dammaged and it needs
   to see `.L' at the start of a symbol or else it will be put into the
   linker symbol table.
*/

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
do {									\
  if ((int) (NUM) >= 0)							\
    sprintf (LABEL, "*.L%s%d", PREFIX, NUM);				\
  else									\
    sprintf (LABEL, "*.L%s", PREFIX);					\
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

#define TEXT_SECTION_ASM_OP	"\t.section\t\".text\""
#define DATA_SECTION_ASM_OP	"\t.section\t\".data\""
#define BSS_SECTION_ASM_OP	"\t.section\t\".bss\""
#define CONST_SECTION_ASM_OP	"\t.section\t\".rodata\""
#define INIT_SECTION_ASM_OP	"\t.section\t\".init\",#alloc"
#define CTORS_SECTION_ASM_OP    "\t.section\t\".ctors\",#alloc"
#define DTORS_SECTION_ASM_OP    "\t.section\t\".dtors\",#alloc"
