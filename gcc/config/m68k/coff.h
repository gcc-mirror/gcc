/* Definitions of target machine for GNU compiler.
   m68k series COFF object files and debugging, version.
   Copyright (C) 1994, 1996, 1997, 2000, 2002 Free Software Foundation, Inc.

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

/* This file is included after m68k.h by CPU COFF specific files.  It
   is not a complete target itself.  */

/* Generate sdb debugging information.  */

#define SDB_DEBUGGING_INFO 1

/* Output DBX (stabs) debugging information if using -gstabs.  */

#include "dbxcoff.h"

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

#define ASM_RETURN_CASE_JUMP				\
  do {							\
    if (TARGET_5200)					\
      {							\
	if (ADDRESS_REG_P (operands[0]))		\
	  return "jmp %%pc@(2,%0:l)";			\
	else						\
	  return "ext%.l %0\n\tjmp %%pc@(2,%0:l)";	\
      }							\
    else						\
      return "jmp %%pc@(2,%0:w)";			\
  } while (0)

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

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as uninitialized global
   data.  */

#define BSS_SECTION_ASM_OP	"\t.section\t.bss"

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* Switch into a generic section.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  m68k_coff_asm_named_section

/* Don't assume anything about startfiles.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""
