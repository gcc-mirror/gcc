/* Definitions for ELF assembler support.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* So we can conditionalize small amounts of code in pa.c or pa.md.  */
#define OBJ_ELF

/* Dummy definitions.  We do not care about this stuff for ELF.  */
#define TEXT_SPACE_P(DECL) 0
#define FUNCTION_NAME_P(NAME) 0

#define TEXT_SECTION_ASM_OP "\t.text"
#define DATA_SECTION_ASM_OP "\t.data"
#define BSS_SECTION_ASM_OP "\t.section\t.bss"

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)  \
  (fputs ("\t.globl ", FILE), assemble_name (FILE, NAME), fputs ("\n", FILE))

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
{ fputs ("\t.word ", FILE);			\
  output_addr_const (FILE, (VALUE));		\
  fputs ("\n", FILE);}

#define ASM_OUTPUT_DOUBLE_INT(FILE,VALUE)  \
{ fputs ("\t.dword ", FILE);			\
  output_addr_const (FILE, (VALUE));		\
  fputs ("\n", FILE);}

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
do {  \
     if (TARGET_PA_20) \
       fputs("\t.LEVEL 2.0\n", FILE); \
     else if (TARGET_PA_11) \
       fputs("\t.LEVEL 1.1\n", FILE); \
     else \
       fputs("\t.LEVEL 1.0\n", FILE); \
     if (profile_flag)\
       fprintf (FILE, "\t.IMPORT _mcount, CODE\n");\
     if (write_symbols != NO_DEBUG) \
       output_file_directive ((FILE), main_input_filename); \
   } while (0)
