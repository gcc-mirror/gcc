/* Definitions of target machine for gcc for Hitachi / SuperH SH using ELF.
   Copyright (C) 1996, 1997, 2000, 2001, 2002 Free Software Foundation, Inc.
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

/* Generate DWARF2 debugging information and make it the default */
#define DWARF2_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* use a more compact format for line information */
#define DWARF2_ASM_LINE_DEBUG_INFO 1

/* WCHAR_TYPE / WCHAR_TYPE_SIZE are defined to long int / BITS_PER_WORD in
   svr4.h, but these work out as 64 bit for shmedia64.  */
#undef WCHAR_TYPE
/* #define WCHAR_TYPE (TARGET_SH5 ? "int" : "long int") */
#define WCHAR_TYPE SH_ELF_WCHAR_TYPE
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


/* The prefix to add to user-visible assembler symbols.  */

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
#define TARGET_OBJFMT_CPP_BUILTINS() builtin_define ("__ELF__")

#undef SIZE_TYPE
#define SIZE_TYPE (TARGET_SH5 ? "long unsigned int" : "unsigned int")

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_SH5 ? "long int" : "int")
/* Pass -ml and -mrelax to the assembler and linker.  */
#undef ASM_SPEC
#define ASM_SPEC  "%(subtarget_asm_endian_spec) %{mrelax:-relax} \
%{m5-compact:--isa=SHcompact} %{m5-compact-nofpu:--isa=SHcompact} \
%{m5-32media:--isa=SHmedia --abi=32} %{m5-32media-nofpu:--isa=SHmedia --abi=32} \
%{m5-64media:--isa=SHmedia --abi=64} %{m5-64media-nofpu:--isa=SHmedia --abi=64}"

#undef LINK_SPEC
#define LINK_SPEC SH_LINK_SPEC
#undef LINK_EMUL_PREFIX
#if TARGET_ENDIAN_DEFAULT == LITTLE_ENDIAN_BIT
#define LINK_EMUL_PREFIX "sh%{!mb:l}elf"
#else
#define LINK_EMUL_PREFIX "sh%{ml:l}elf"
#endif

/* svr4.h undefined DBX_REGISTER_NUMBER, so we need to define it
   again.  */
#define DBX_REGISTER_NUMBER(REGNO) SH_DBX_REGISTER_NUMBER (REGNO)

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

/* ASM_OUTPUT_CASE_LABEL is defined in elfos.h.  With it,
   a redundant .align was generated.  */
#undef  ASM_OUTPUT_CASE_LABEL
