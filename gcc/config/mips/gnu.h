/* Definitions of target machine for GNU compiler.  MIPS GNU Hurd version.
   Copyright (C) 1995, 1996, 1999 Free Software Foundation, Inc.

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

#define TARGET_DEFAULT MASK_GAS

#include <mips/mips.h>

#undef SWITCH_TAKES_ARG
#undef ASM_FILE_END
#undef ASM_OUTPUT_IDENT
#undef ASM_OUTPUT_SOURCE_LINE
#undef READONLY_DATA_SECTION
#undef SELECT_SECTION
#undef ASM_DECLARE_FUNCTION_NAME
#undef ASM_DECLARE_OBJECT_NAME
/* #undef PREFERRED_DEBUGGING_TYPE */

#include <svr4.h>

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX
#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (MIPS GNU/ELF)");

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
  do {									\
	mips_asm_file_start (FILE);					\
	fprintf (FILE, "\t.version\t\"01.01\"\n");			\
  } while (0)

#undef ASM_FILE_END
#define ASM_FILE_END(FILE)						\
  do {				 					\
	mips_asm_file_end(FILE);					\
	if (!flag_no_ident)						\
	  fprintf ((FILE), "\t%s\t\"GCC: (GNU) %s\"\n",			\
		   IDENT_ASM_OP, version_string);			\
  } while (0)

#undef ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINE)				\
  do {									\
      ++sym_lineno;							\
      fprintf ((FILE), ".LM%d:\n\t%s %d,0,%d,.LM%d\n",			\
	       sym_lineno, ASM_STABN_OP, N_SLINE, (LINE), sym_lineno);	\
  } while (0)

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)			\
  do {									\
    extern FILE *asm_out_text_file;					\
									\
    if (TARGET_GP_OPT)							\
      STREAM = asm_out_text_file;					\
    fprintf (STREAM, "\t%s\t ", TYPE_ASM_OP);				\
    assemble_name (STREAM, NAME);					\
    putc (',', STREAM);							\
    fprintf (STREAM, TYPE_OPERAND_FMT, "function");			\
    putc ('\n', STREAM);						\
    ASM_DECLARE_RESULT (STREAM, DECL_RESULT (DECL));			\
    HALF_PIC_DECLARE (NAME);						\
  } while (0)

/* Switch  Recognition by gcc.c.  Add -G xx support */
#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)						\
  (DEFAULT_SWITCH_TAKES_ARG(CHAR) || (CHAR) == 'G')

#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) mips_dbx_regno[ (REGNO) ]

#define MIPS_GNU

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmips -Acpu(mips) -Amachine(mips) \
-Dunix -Asystem(unix)  -DMACH -Asystem(mach) -D__GNU__ -Asystem(gnu) \
-DMIPSEB -DR3000 -D_MIPSEB -D_R3000 \
-D_MIPS_SZINT=32 -D_MIPS_SZLONG=32 -D_MIPS_SZPTR=32"

#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{!shared: %{!non_shared: %{!call_shared: -non_shared}}} \
-systype /gnu/ "
		    
#undef LIB_SPEC
#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} -lc crtn.o%s"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  "%{pg:gcrt0.o%s} %{!pg:%{p:gcrt0.o%s} %{!p:crt0.o%s}} %{static:-static}"

#undef MACHINE_TYPE
#define MACHINE_TYPE "GNU MIPS/ELF"

#undef YES_UNDERSCORE

#undef SDB_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO
#define DWARF_DEBUGGING_INFO

#define NO_MIPS_SELECT_SECTION

/* Get machine-independent configuration parameters for the GNU system.  */
#include <gnu.h>
