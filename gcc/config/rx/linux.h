/* GCC backend definitions for the rx-linux
   Copyright (C) 2019 Yoshinori Sato
   Based on rx.h

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


#include "config/rx/rx.h"

#undef TARGET_CPU_CPP_BUILTINS
#define TARGET_CPU_CPP_BUILTINS()               \
  do                                            \
    {                                           \
      builtin_define ("__RX__"); 		\
      builtin_assert ("cpu=RX"); 		\
      if (rx_cpu_type == RX610)			\
        builtin_assert ("machine=RX610");	\
     else					\
        builtin_assert ("machine=RX600");	\
      						\
      if (TARGET_BIG_ENDIAN_DATA)		\
	builtin_define ("__RX_BIG_ENDIAN__");	\
      else					\
	builtin_define ("__RX_LITTLE_ENDIAN__");\
						\
      if (TARGET_64BIT_DOUBLES)			\
	builtin_define ("__RX_64BIT_DOUBLES__");\
      else					\
	builtin_define ("__RX_32BIT_DOUBLES__");\
      						\
      if (ALLOW_RX_FPU_INSNS)			\
	builtin_define ("__RX_FPU_INSNS__");	\
						\
    }                                           \
  while (0)

#undef  CC1_SPEC
#define CC1_SPEC "\
  %{mcpu=rx200:%{fpu:%erx200 cpu does not have FPU hardware}}"

#undef  ASM_SPEC
#define ASM_SPEC "\
%{mbig-endian-data:-mbig-endian-data} \
%{m64bit-doubles:-m64bit-doubles} \
%{!m64bit-doubles:-m32bit-doubles} \
%{msmall-data-limit*:-msmall-data-limit} \
%{mrelax:-relax} \
"

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP	      			\
   "\t.section .data,\"aw\",@progbits\n\t.p2align 2"

#undef SDATA_SECTION_ASM_OP
#define SDATA_SECTION_ASM_OP	      			\
   "\t.section .data2,\"aw\",@progbits\n\t.p2align 1"

#undef  READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP  			\
   "\t.section .rodata,\"a\",@progbits\n\t.p2align 2"

#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP	      			\
   "\t.section .bss\n\t.p2align 2"

#undef SBSS_SECTION_ASM_OP
#define SBSS_SECTION_ASM_OP	      			\
   "\t.section .bss2\n\t.p2align 1"

/* The following definitions are conditional depending upon whether the
   compiler is being built or crtstuff.c is being compiled by the built
   compiler.  */
#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP	      \
  "\t.section .text,\"ax\""
#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	      \
  "\t.section\t.init_array,\"aw\",@init_array"
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	      \
  "\t.section\t.fini_array,\"aw\",@fini_array"
#undef INIT_ARRAY_SECTION_ASM_OP
#define INIT_ARRAY_SECTION_ASM_OP   \
  "\t.section\t.init_array,\"aw\",@init_array"
#undef FINI_ARRAY_SECTION_ASM_OP
#define FINI_ARRAY_SECTION_ASM_OP   \
  "\t.section\t.fini_array,\"aw\",@fini_array"

#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP		"\t.global\t"
#undef  USER_LABEL_PREFIX

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(STREAM, LOG)		\
  do						\
    {						\
      if ((LOG) == 0)				\
        break;					\
      fprintf (STREAM, "\t.balign %d\n", 1 << (LOG));	\
    }						\
  while (0)

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "\t.long .L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.
   Note: The local label referenced by the "1b" below is emitted by
   the tablejump insn.  */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t.long .L%d - 1b\n", VALUE)

#undef ASM_OUTPUT_SIZE_DIRECTIVE
#define ASM_OUTPUT_SIZE_DIRECTIVE(STREAM, NAME, SIZE)			\
  do									\
    {									\
      HOST_WIDE_INT size_ = (SIZE);					\
									\
      fputs (SIZE_ASM_OP, STREAM);					\
      assemble_name (STREAM, NAME);					\
      fprintf (STREAM, ", " HOST_WIDE_INT_PRINT_DEC "\n", size_);	\
    }									\
  while (0)

#undef ASM_OUTPUT_MEASURED_SIZE
#define ASM_OUTPUT_MEASURED_SIZE(STREAM, NAME)				\
  do									\
    {									\
      fputs (SIZE_ASM_OP, STREAM);					\
      assemble_name (STREAM, NAME);					\
      fputs (", .-", STREAM);						\
      assemble_name (STREAM, NAME);					\
      putc ('\n', STREAM);						\
    }									\
  while (0)

#undef ASM_OUTPUT_TYPE_DIRECTIVE
#define ASM_OUTPUT_TYPE_DIRECTIVE(STREAM, NAME, TYPE)			\
  do									\
    {									\
      fputs (TYPE_ASM_OP, STREAM);					\
      assemble_name (STREAM, NAME);					\
      fputs (", ", STREAM);						\
      fprintf (STREAM, TYPE_OPERAND_FMT, TYPE);				\
      putc ('\n', STREAM);						\
    }									\
  while (0)

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)		\
  do								\
    {								\
      sprintf (LABEL, "*.%s%u", PREFIX, (unsigned) (NUM));			\
    }								\
  while (0)

#undef  ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)			\
  do								\
    {								\
      default_elf_asm_output_external (FILE, DECL, NAME);	\
    }								\
  while (0)

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      fprintf ((FILE), "%s", COMMON_ASM_OP);				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED ",%u\n",	\
	       (SIZE), (ALIGN) / BITS_PER_UNIT);			\
    }									\
  while (0)

#undef  SKIP_ASM_OP
#define SKIP_ASM_OP   "\t.zero\t"

#undef  ASM_OUTPUT_LIMITED_STRING
#define ASM_OUTPUT_LIMITED_STRING(FILE, STR)		\
  do							\
    {							\
      const unsigned char *_limited_str =		\
	(const unsigned char *) (STR);			\
      unsigned ch;					\
							\
      fprintf ((FILE), "\t.string\t\"");		\
							\
      for (; (ch = *_limited_str); _limited_str++)	\
        {						\
	  int escape;					\
							\
	  switch (escape = ESCAPES[ch])			\
	    {						\
	    case 0:					\
	      putc (ch, (FILE));			\
	      break;					\
	    case 1:					\
	      fprintf ((FILE), "\\%03o", ch);		\
	      break;					\
	    default:					\
	      putc ('\\', (FILE));			\
	      putc (escape, (FILE));			\
	      break;					\
	    }						\
        }						\
							\
      fprintf ((FILE), "\"\n");				\
    }							\
  while (0)

#undef  PREFERRED_DEBUGGING_TYPE

#undef TARGET_AS100_SYNTAX
#define TARGET_AS100_SYNTAX 0
