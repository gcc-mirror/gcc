/* Definitions of target machine for gcc for Hitachi Super-H using ELF.
   Copyright (C) 1996, 1997, 2000 Free Software Foundation, Inc.
   Contributed by Ian Lance Taylor <ian@cygnus.com>.

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

/* Mostly like the regular SH configuration.  */
#include "sh/sh.h"

/* No SDB debugging info.  */
#undef SDB_DEBUGGING_INFO

/* Undefine some macros defined in both sh.h and svr4.h.  */
#undef IDENT_ASM_OP
#undef ASM_FILE_END
#undef ASM_OUTPUT_SOURCE_LINE
#undef DBX_OUTPUT_MAIN_SOURCE_FILE_END
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#undef ASM_OUTPUT_SECTION_NAME
#undef ASM_OUTPUT_CONSTRUCTOR
#undef ASM_OUTPUT_DESTRUCTOR
#undef ASM_DECLARE_FUNCTION_NAME
#undef PREFERRED_DEBUGGING_TYPE
#undef MAX_OFILE_ALIGNMENT

/* Be ELF-like.  */
#include "svr4.h"

/* The prefix to add to user-visible assembler symbols.
   Note that svr4.h redefined it from the original value (that we want)
   in sh.h */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) do {				\
  output_file_directive ((FILE), main_input_filename);		\
  if (TARGET_LITTLE_ENDIAN)					\
    fprintf ((FILE), "\t.little\n");				\
} while (0)



/* Let code know that this is ELF.  */
#define CPP_PREDEFINES "-D__sh__ -D__ELF__ -Acpu(sh) -Amachine(sh)"

/* Pass -ml and -mrelax to the assembler and linker.  */
#undef ASM_SPEC
#define ASM_SPEC  "%{ml:-little} %{mrelax:-relax}"

#undef LINK_SPEC
#define LINK_SPEC "%{ml:-m shlelf} %{mrelax:-relax}"

/* svr4.h undefined DBX_REGISTER_NUMBER, so we need to define it
   again.  */
#define DBX_REGISTER_NUMBER(REGNO)	\
  (((REGNO) >= 22 && (REGNO) <= 39) ? ((REGNO) + 1) : (REGNO))

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM) \
  sprintf ((STRING), "*%s%s%ld", LOCAL_LABEL_PREFIX, (PREFIX), (long)(NUM))

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM) \
  asm_fprintf ((FILE), "%L%s%d:\n", (PREFIX), (NUM))

#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)				\
do									\
  {									\
    static int sym_lineno = 1;						\
    asm_fprintf ((file), ".stabn 68,0,%d,%LLM%d-",			\
	     (line), sym_lineno);					\
    assemble_name ((file),						\
		   XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));\
    asm_fprintf ((file), "\n%LLM%d:\n", sym_lineno);			\
    sym_lineno += 1;							\
  }									\
while (0)

#undef DBX_OUTPUT_MAIN_SOURCE_FILE_END
#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)			\
do {									\
  text_section ();							\
  fprintf ((FILE), "\t.stabs \"\",%d,0,0,Letext\nLetext:\n", N_SO);	\
} while (0)

/* Arrange to call __main, rather than using crtbegin.o and crtend.o
   and relying on .init and .fini being executed at appropriate times.  */
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
#undef STARTFILE_SPEC
#undef ENDFILE_SPEC

/* HANDLE_SYSV_PRAGMA (defined by svr4.h) takes precedence over HANDLE_PRAGMA.
   We want to use the HANDLE_PRAGMA from sh.h.  */
#undef HANDLE_SYSV_PRAGMA
