/* Definitions of target machine for GNU compiler.
   m68k series COFF object files and debugging, version.
   Copyright (C) 1987, 1988, 1991, 1994 Free Software Foundation, Inc.

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

/* This file is included after m68k.h by CPU COFF specific files.  It
   is not a complete target itself.  */

/* Generate sdb debugging information.  */

#undef DBX_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO

/* Output DBX (stabs) debugging information if using -gstabs.  */

#define DBX_DEBUGGING_INFO

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG

/* COFF symbols don't start with an underscore.  */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* Use a prefix for local labels, just to be on the save side.  */

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

/* Use a register prefix to avoid clashes with external symbols (classic
   example: `extern char PC;' in termcap).  */

#undef REGISTER_PREFIX
#define REGISTER_PREFIX "%"

/* In the machine description we can't use %R, because it will not be seen
   by ASM_FPRINTF.  (Isn't that a design bug?).  */

#undef REGISTER_PREFIX_MD
#define REGISTER_PREFIX_MD "%%"

/* config/m68k.md has an explicit reference to the program counter,
   prefix this by the register prefix.  */

#define ASM_RETURN_CASE_JUMP    return "jmp %%pc@(2,%0:w)"

/* Here are the new register names.  */

#undef REGISTER_NAMES
#ifndef SUPPORT_SUN_FPA
#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",	\
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%sp",	\
 "%fp0", "%fp1", "%fp2", "%fp3", "%fp4", "%fp5", "%fp6", "%fp7" }
#else /* SUPPORTED_SUN_FPA */
#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",	\
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%sp",	\
 "%fp0", "%fp1", "%fp2", "%fp3", "%fp4", "%fp5", "%fp6", "%fp7", \
 "%fpa0", "%fpa1", "%fpa2", "%fpa3", "%fpa4", "%fpa5", "%fpa6", "%fpa7", \
 "%fpa8", "%fpa9", "%fpa10", "%fpa11", "%fpa12", "%fpa13", "%fpa14", "%fpa15", \
 "%fpa16", "%fpa17", "%fpa18", "%fpa19", "%fpa20", "%fpa21", "%fpa22", "%fpa23", \
 "%fpa24", "%fpa25", "%fpa26", "%fpa27", "%fpa28", "%fpa29", "%fpa30", "%fpa31" }
#endif /* defined SUPPORT_SUN_FPA */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
  output_file_directive ((FILE), main_input_filename)

/* Support the ctors and dtors sections for g++.  */

#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"x\""
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"x\""

/* A list of other sections which the compiler might be "in" at any
   given time.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_ctors, in_dtors

/* A list of extra section function definitions.  */

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION

#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

#define INT_ASM_OP ".long"

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

/* Don't assume anything about startfiles.  */

#define STARTFILE_SPEC ""
