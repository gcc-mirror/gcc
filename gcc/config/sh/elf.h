/* Definitions of target machine for gcc for Hitachi / SuperH SH using ELF.
   Copyright (C) 1996, 1997, 2000, 2001 Free Software Foundation, Inc.
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

/* Undefine some macros defined in both sh.h and svr4.h.  */
#undef IDENT_ASM_OP
#undef ASM_FILE_END
#undef ASM_OUTPUT_SOURCE_LINE
#undef DBX_OUTPUT_MAIN_SOURCE_FILE_END
#undef TARGET_ASM_NAMED_SECTION
#undef ASM_DECLARE_FUNCTION_NAME
#undef MAX_OFILE_ALIGNMENT
#undef SIZE_TYPE
#undef PTRDIFF_TYPE

/* Be ELF-like.  */
/* TODO: convert includes to ${tm_file} list in config.gcc.  */
#include "dbxelf.h"
#include "elfos.h"
#include "svr4.h"

/* No SDB debugging info.  */
#undef SDB_DEBUGGING_INFO

/* Generate DWARF2 debugging information and make it the default */
#undef DWARF2_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* use a more compact format for line information */
#define DWARF2_ASM_LINE_DEBUG_INFO 1

/* WCHAR_TYPE_SIZE is defined to BITS_PER_WORD in svr4.h, but
   BITS_PER_WORD isn't constant any more.  Fortunately, on no SH
   platform is it wider than 32-bits.  */
#define MAX_WCHAR_TYPE_SIZE 32

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
/* We also need to show the text section with the proper	\
   attributes as in TEXT_SECTION_ASM_OP, before dwarf2out	\
   emits it without attributes in TEXT_SECTION, else GAS	\
   will complain.  We can teach GAS specifically about the	\
   default attributes for our choice of text section, but	\
   then we would have to change GAS again if/when we change	\
   the text section name.  */					\
   fprintf ((FILE), "%s\n", TEXT_SECTION_ASM_OP);		\
  if (TARGET_LITTLE_ENDIAN)					\
    fprintf ((FILE), "\t.little\n");				\
} while (0)



/* Let code know that this is ELF.  */
#define CPP_PREDEFINES "-D__sh__ -D__ELF__ -Acpu=sh -Amachine=sh"

#undef SIZE_TYPE
#define SIZE_TYPE (TARGET_SH5 ? "long unsigned int" : "unsigned int")

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_SH5 ? "long int" : "int")
/* Pass -ml and -mrelax to the assembler and linker.  */
#undef ASM_SPEC
#define ASM_SPEC  "%{ml:-little} %{mrelax:-relax} \
%{m5-compact:--isa=SHcompact} %{m5-compact-nofpu:--isa=SHcompact} \
%{m5-32media:--isa=SHmedia --abi=32} %{m5-32media-nofpu:--isa=SHmedia --abi=32} \
%{m5-64media:--isa=SHmedia --abi=64} %{m5-64media-nofpu:--isa=SHmedia --abi=64}"

#undef LINK_SPEC
#define LINK_SPEC " \
%{m5-compact:%{!ml:-m shelf32} %{ml:-m shlelf32}} \
%{m5-compact-nofpu:%{!ml:-m shelf32} %{ml:-m shlelf32}} \
%{m5-32media:%{!ml:-m shelf32} %{ml:-m shlelf32}} \
%{m5-32media-nofpu:%{!ml:-m shelf32} %{ml:-m shlelf32}} \
%{m5-64media:%{!ml:-m shelf64} %{ml:-m shlelf64}} \
%{m5-64media-nofpu:%{!ml:-m shelf64} %{ml:-m shlelf64}} \
%{!m5-64media:%{!m5-64media-nofpu:%{!m5-32media:%{!m5-32media-nofpu:%{!m5-compact:%{!m5-compact-nofpu:%{ml:-m shlelf}}}}}}} \
%{mrelax:-relax}"

/* svr4.h undefined DBX_REGISTER_NUMBER, so we need to define it
   again.  */
#define DBX_REGISTER_NUMBER(REGNO)					\
  (GENERAL_REGISTER_P (REGNO)						\
   ? ((REGNO) - FIRST_GENERAL_REG)					\
   : FP_REGISTER_P (REGNO)						\
   ? ((REGNO) - FIRST_FP_REG + (TARGET_SH5 ? (TARGET_SHCOMPACT ? 245	\
					      : 77) : 25))		\
   : XD_REGISTER_P (REGNO)						\
   ? ((REGNO) - FIRST_XD_REG + (TARGET_SH5 ? 289 : 87))			\
   : TARGET_REGISTER_P (REGNO)						\
   ? ((REGNO) - FIRST_TARGET_REG + 68)					\
   : (REGNO) == PR_REG							\
   ? (TARGET_SH5 ? 241 : 17)						\
   : (REGNO) == T_REG							\
   ? (TARGET_SH5 ? 242 : 18)						\
   : (REGNO) == GBR_REG							\
   ? (TARGET_SH5 ? 238 : 19)						\
   : (REGNO) == MACH_REG						\
   ? (TARGET_SH5 ? 239 : 20)						\
   : (REGNO) == MACL_REG						\
   ? (TARGET_SH5 ? 240 : 21)						\
   : (REGNO) == FPUL_REG						\
   ? (TARGET_SH5 ? 244 : 23)						\
   : (abort(), -1))

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

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: crt1.o%s} crti.o%s \
   %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"
