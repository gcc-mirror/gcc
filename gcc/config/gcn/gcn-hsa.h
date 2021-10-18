/* Copyright (C) 2016-2021 Free Software Foundation, Inc.

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

/* Avoid the default in ../../gcc.c, which adds "-pthread", which is not
   supported for gcn.  */
#define GOMP_SELF_SPECS ""

#ifdef HAVE_GCN_XNACK_FIJI
#define X_FIJI
#else
#define X_FIJI "!march=*:;march=fiji:;"
#endif
#ifdef HAVE_GCN_XNACK_GFX900
#define X_900
#else
#define X_900 "march=gfx900:;"
#endif
#ifdef HAVE_GCN_XNACK_GFX906
#define X_906
#else
#define X_906 "march=gfx906:;"
#endif
#ifdef HAVE_GCN_XNACK_GFX908
#define X_908
#else
#define X_908 "march=gfx908:;"
#endif

/* These targets can't have SRAM-ECC, even if a broken assembler allows it.  */
#define S_FIJI "!march=*:;march=fiji:;"
#define S_900 "march=gfx900:;"
#define S_906 "march=gfx906:;"
#ifdef HAVE_GCN_SRAM_ECC_GFX908
#define S_908
#else
#define S_908 "march=gfx908:;"
#endif

#ifdef HAVE_GCN_ASM_V3_SYNTAX
#define SRAMOPT "!msram-ecc=off:-mattr=+sram-ecc;:-mattr=-sram-ecc"
#endif
#ifdef HAVE_GCN_ASM_V4_SYNTAX
/* In HSACOv4 no attribute setting means the binary supports "any" hardware
   configuration.  The name of the attribute also changed.  */
#define SRAMOPT "msram-ecc=on:-mattr=+sramecc;msram-ecc=off:-mattr=-sramecc"
#endif
#if !defined(SRAMOPT) && !defined(IN_LIBGCC2)
#error "No assembler syntax configured"
#endif

#ifdef HAVE_GCN_ASM_V4_SYNTAX
/* FIJI cards don't seem to support drivers new enough to allow HSACOv4.  */
#define HSACO3_SELECT_OPT \
    "%{!march=*|march=fiji:--amdhsa-code-object-version=3} "
#else
#define HSACO3_SELECT_OPT
#endif

/* These targets can't have SRAM-ECC, even if a broken assembler allows it.  */
#define DRIVER_SELF_SPECS \
  "%{march=fiji|march=gfx900|march=gfx906:%{!msram-ecc=*:-msram-ecc=off}}"

/* Use LLVM assembler and linker options.  */
#define ASM_SPEC  "-triple=amdgcn--amdhsa "  \
		  "%:last_arg(%{march=*:-mcpu=%*}) " \
		  HSACO3_SELECT_OPT \
		  "%{" X_FIJI X_900 X_906 X_908 \
		     "mxnack:-mattr=+xnack;:-mattr=-xnack} " \
		  "%{" S_FIJI S_900 S_906 S_908 SRAMOPT "} " \
		  "-filetype=obj"
#define LINK_SPEC "--pie --export-dynamic"
#define LIB_SPEC  "-lc"

/* Provides a _start symbol to keep the linker happy.  */
#define STARTFILE_SPEC "crt0.o%s"
#define ENDFILE_SPEC   ""
#define STANDARD_STARTFILE_PREFIX_2 ""

/* The LLVM assembler rejects multiple -mcpu options, so we must drop
   all but the last.  */
extern const char *last_arg_spec_function (int argc, const char **argv);
#define EXTRA_SPEC_FUNCTIONS	\
    { "last_arg", last_arg_spec_function },

#undef LOCAL_INCLUDE_DIR

/* FIXME: Review debug info settings.
 *        In particular, EH_FRAME_THROUGH_COLLECT2 is probably the wrong
 *        thing but stuff fails to build without it.
 *        (Debug info is not a big deal until we get a debugger.)  */
#define PREFERRED_DEBUGGING_TYPE   DWARF2_DEBUG
#define DWARF2_DEBUGGING_INFO      1
#define DWARF2_ASM_LINE_DEBUG_INFO 1
#define EH_FRAME_THROUGH_COLLECT2  1
#define DBX_REGISTER_NUMBER(REGNO) gcn_dwarf_register_number (REGNO)
