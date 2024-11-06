/* Copyright (C) 2016-2024 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef OBJECT_FORMAT_ELF
 #error elf.h included before elfos.h
#endif

#define TEXT_SECTION_ASM_OP "\t.text"
#define BSS_SECTION_ASM_OP  "\t.bss"
#define GLOBAL_ASM_OP       "\t.globl\t"
#define DATA_SECTION_ASM_OP "\t.data\t"
#define SET_ASM_OP          "\t.set\t"
#define LOCAL_LABEL_PREFIX  "."
#define USER_LABEL_PREFIX   ""
#define ASM_COMMENT_START   ";"
#define TARGET_ASM_NAMED_SECTION default_elf_asm_named_section

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
	    asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
  gcn_hsa_declare_function_name ((FILE), (NAME), (DECL))

/* Unlike GNU as, the LLVM assembler uses log2 alignments.  */
#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGNMENT)	  \
 (fprintf ((FILE), "%s", COMMON_ASM_OP),			  \
  assemble_name ((FILE), (NAME)),				  \
  fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED ",%u\n",	  \
	   (SIZE) > 0 ? (SIZE) : 1, exact_log2 ((ALIGNMENT) / BITS_PER_UNIT)))

#define ASM_OUTPUT_LABEL(FILE,NAME) \
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

#define ASM_OUTPUT_LABELREF(FILE, NAME) \
  asm_fprintf (FILE, "%U%s", default_strip_name_encoding (NAME))

extern unsigned int gcn_local_sym_hash (const char *name);

#define ASM_OUTPUT_SYMBOL_REF(FILE, X) gcn_asm_output_symbol_ref (FILE, X)

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t.word .L%d-.L%d\n", VALUE, REL)

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "\t.word .L%d\n", VALUE)

#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  do { if (LOG!=0) fprintf (FILE, "\t.align\t%d\n", 1<<(LOG)); } while (0)
#define ASM_OUTPUT_ALIGN_WITH_NOP(FILE,LOG)	       \
  do {						       \
    if (LOG!=0)					       \
      fprintf (FILE, "\t.p2alignl\t%d, 0xBF800000"     \
	       " ; Fill value is 's_nop 0'\n", (LOG)); \
  } while (0)

#define ASM_APP_ON  ""
#define ASM_APP_OFF ""

/* Avoid the default in ../../gcc.cc, which adds "-pthread", which is not
   supported for gcn.  */
#define GOMP_SELF_SPECS ""

#include "gcn-device-macros.h"

/* Use LLVM assembler and linker options.  */
#define ASM_SPEC  "-triple=amdgcn--amdhsa "  \
		  "%{march=*:-mcpu=%*} " \
		  ABI_VERSION_OPT \
		  XNACKOPT \
		  SRAMOPT \
		  WAVE64OPT \
		  CUMODEOPT \
		  "-filetype=obj"
#define LINK_SPEC "--pie --export-dynamic"
#define LIB_SPEC  "-lc"

/* Provides a _start symbol to keep the linker happy.  */
#define STARTFILE_SPEC "crt0.o%s"
#define ENDFILE_SPEC   ""
#define STANDARD_STARTFILE_PREFIX_2 ""

#undef LOCAL_INCLUDE_DIR

/* FIXME: Review debug info settings.
 *        In particular, EH_FRAME_THROUGH_COLLECT2 is probably the wrong
 *        thing but stuff fails to build without it.
 *        (Debug info is not a big deal until we get a debugger.)  */
#define PREFERRED_DEBUGGING_TYPE   DWARF2_DEBUG
#define DWARF2_DEBUGGING_INFO      1
#define DWARF2_ASM_LINE_DEBUG_INFO 1
#define EH_FRAME_THROUGH_COLLECT2  1
#define DEBUGGER_REGNO(REGNO) gcn_dwarf_register_number (REGNO)
