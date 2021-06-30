// SPDX-License-Identifier: GPL-3.0-or-later
/* Definitions of target machine for GNU compiler,
   for some generic XCOFF file format
   Copyright (C) 2001-2021 Free Software Foundation, Inc.

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

#define TARGET_OBJECT_FORMAT OBJECT_XCOFF

/* The RS/6000 uses the XCOFF format.  */
#define XCOFF_DEBUGGING_INFO 1

/* Define if the object format being used is COFF or a superset.  */
#define OBJECT_FORMAT_COFF

/* Define the magic numbers that we recognize as COFF.
 
    AIX 4.3 adds U803XTOCMAGIC (0757) for 64-bit objects and AIX V5 adds
    U64_TOCMAGIC (0767), but collect2.c does not include files in the
    correct order to conditionally define the symbolic name in this macro.
 
    The AIX linker accepts import/export files as object files,
    so accept "#!" (0x2321) magic number.  */
#define MY_ISCOFF(magic) \
  ((magic) == U802WRMAGIC || (magic) == U802ROMAGIC \
   || (magic) == U802TOCMAGIC || (magic) == 0757 || (magic) == 0767 \
   || (magic) == 0x2321)

/* We don't have GAS for the RS/6000 yet, so don't write out special
    .stabs in cc1plus.  */

#define FASCIST_ASSEMBLER

/* We define this to prevent the name mangler from putting dollar signs into
   function names.  */

#define NO_DOLLAR_IN_LABEL

/* We define this to 0 so that gcc will never accept a dollar sign in a
   variable name.  This is needed because the AIX assembler will not accept
   dollar signs.  */

#define DOLLARS_IN_IDENTIFIERS 0

/* AIX .align pseudo-op accept value from 0 to 12, corresponding to
   log base 2 of the alignment in bytes; 12 = 4096 bytes = 32768 bits.  */

#define MAX_OFILE_ALIGNMENT 32768

/* Default alignment factor for csect directives, chosen to honor
   BIGGEST_ALIGNMENT.  */
#define XCOFF_CSECT_DEFAULT_ALIGNMENT_STR "4"

/* Return nonzero if this entry is to be written into the constant
   pool in a special way.  We do so if this is a SYMBOL_REF, LABEL_REF
   or a CONST containing one of them.  If -mfp-in-toc (the default),
   we also do this for floating-point constants.  We actually can only
   do this if the FP formats of the target and host machines are the
   same, but we can't check that since not every file that uses these
   target macros includes real.h.  We also do this when we can write the
   entry into the TOC and the entry is not larger than a TOC entry.  */

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X, MODE)			\
  (TARGET_TOC								\
   && (SYMBOL_REF_P (X)							\
       || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS	\
	   && SYMBOL_REF_P (XEXP (XEXP (X, 0), 0)))			\
       || GET_CODE (X) == LABEL_REF					\
       || (CONST_INT_P (X)						\
	   && GET_MODE_BITSIZE (MODE) <= GET_MODE_BITSIZE (Pmode))	\
       || (CONST_DOUBLE_P (X)						\
	   && (TARGET_MINIMAL_TOC					\
	       || (SCALAR_FLOAT_MODE_P (GET_MODE (X))			\
		   && ! TARGET_NO_FP_IN_TOC)))))

#undef TARGET_DEBUG_UNWIND_INFO
#define TARGET_DEBUG_UNWIND_INFO  rs6000_xcoff_debug_unwind_info
#define TARGET_ASM_OUTPUT_ANCHOR  rs6000_xcoff_asm_output_anchor
#define TARGET_ASM_GLOBALIZE_DECL_NAME  rs6000_xcoff_asm_globalize_decl_name
#define TARGET_ASM_GLOBALIZE_LABEL  rs6000_xcoff_asm_globalize_label
#define TARGET_ASM_INIT_SECTIONS  rs6000_xcoff_asm_init_sections
#define TARGET_ASM_RELOC_RW_MASK  rs6000_xcoff_reloc_rw_mask
#define TARGET_ASM_NAMED_SECTION  rs6000_xcoff_asm_named_section
#define TARGET_ASM_SELECT_SECTION  rs6000_xcoff_select_section
#define TARGET_ASM_SELECT_RTX_SECTION  rs6000_xcoff_select_rtx_section
#define TARGET_ASM_UNIQUE_SECTION  rs6000_xcoff_unique_section
#define TARGET_ASM_FUNCTION_RODATA_SECTION default_no_function_rodata_section
#define TARGET_STRIP_NAME_ENCODING  rs6000_xcoff_strip_name_encoding
#define TARGET_SECTION_TYPE_FLAGS  rs6000_xcoff_section_type_flags
#ifdef HAVE_AS_TLS
#define TARGET_ENCODE_SECTION_INFO rs6000_xcoff_encode_section_info
#endif
#define ASM_OUTPUT_ALIGNED_DECL_COMMON  rs6000_xcoff_asm_output_aligned_decl_common
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL  rs6000_xcoff_asm_output_aligned_decl_common
#define ASM_OUTPUT_ALIGNED_BSS  rs6000_xcoff_asm_output_aligned_decl_common

/* FP save and restore routines.  */
#define	SAVE_FP_PREFIX "._savef"
#define SAVE_FP_SUFFIX ""
#define	RESTORE_FP_PREFIX "._restf"
#define RESTORE_FP_SUFFIX ""

/* Function name to call to do profiling.  */
#undef  RS6000_MCOUNT
#define RS6000_MCOUNT ".__mcount"

/* This outputs NAME to FILE up to the first null or '['.  */

#define RS6000_OUTPUT_BASENAME(FILE, NAME) \
  assemble_name ((FILE), (*targetm.strip_name_encoding) (NAME))

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { RS6000_OUTPUT_BASENAME (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START rs6000_xcoff_file_start
#define TARGET_ASM_FILE_END rs6000_xcoff_file_end
#undef TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE false

/* This macro produces the initial definition of a function name.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  rs6000_xcoff_declare_function_name ((FILE), (NAME), (DECL))
#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  rs6000_xcoff_declare_object_name ((FILE), (NAME), (DECL))

/* Output a reference to SYM on FILE.  */

#define ASM_OUTPUT_SYMBOL_REF(FILE, SYM) \
  rs6000_output_symbol_ref (FILE, SYM)

/* This says how to output an external.
   Dollar signs are converted to underscores.  */

#undef  ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)				\
{ char *buffer = (char *) alloca (strlen (NAME) + 1);			\
  char *p;								\
  int dollar_inside = 0;						\
  strcpy (buffer, NAME);						\
  p = strchr (buffer, '$');						\
  while (p) {								\
    *p = '_';								\
    dollar_inside++;							\
    p = strchr (p + 1, '$');						\
  }									\
  if (dollar_inside) {							\
      fputs ("\t.extern .", FILE);					\
      RS6000_OUTPUT_BASENAME (FILE, buffer);				\
      putc ('\n', FILE);						\
      fprintf (FILE, "\t.rename .%s,\".%s\"\n", buffer, NAME);		\
    }									\
}

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  asm_fprintf ((FILE), "%U%s", rs6000_xcoff_strip_dollar (NAME))

/* This is how to output an internal label prefix.  rs6000.c uses this
   when generating traceback tables.  */

#define ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)   \
  fprintf (FILE, "%s..", PREFIX)

/* This is how to output a label for a jump table.  Arguments are the same as
   for (*targetm.asm_out.internal_label), except the insn for the jump table is
   passed.  */

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 2); (*targetm.asm_out.internal_label) (FILE, PREFIX, NUM); }

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s..%u", rs6000_xcoff_strip_dollar (PREFIX), (unsigned) (NUM))

/* This is how to output an assembler line to define N characters starting
   at P to FILE.  */

#define ASM_OUTPUT_ASCII(FILE, P, N)  output_ascii ((FILE), (P), (N))

/* This is how to advance the location counter by SIZE bytes.  */

#define SKIP_ASM_OP "\t.space "

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "%s" HOST_WIDE_INT_PRINT_UNSIGNED"\n", SKIP_ASM_OP, (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define COMMON_ASM_OP "\t.comm "
#define LOCAL_COMMON_ASM_OP "\t.lcomm "

#ifdef HAVE_AS_TLS
#define ASM_OUTPUT_TLS_COMMON(FILE, DECL, NAME, SIZE)   \
  do { \
       rs6000_xcoff_asm_output_aligned_decl_common ((FILE), (DECL), (NAME), (SIZE), 0); \
  } while (0)
#endif

/* This is how we tell the assembler that two symbols have the same value.  */
#define SET_ASM_OP "\t.set "

/* This is how we tell the assembler to equate two values. 
   The semantic of AIX assembler's .set do not correspond to middle-end expectations.
   We output aliases as alternative symbols in the front of the definition
   via DECLARE_FUNCTION_NAME and DECLARE_OBJECT_NAME.
   We still need to define this macro to let middle-end know that aliases are
   supported.
 */
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2) do { (void) (FILE);	\
						(void) (LABEL1); \
						(void) (LABEL2); } while (0)

/* Used by rs6000_assemble_integer, among others.  */

/* Used by rs6000_assemble_integer, among others.  */
#define DOUBLE_INT_ASM_OP "\t.llong\t"

/* Output before instructions.  */
#define TEXT_SECTION_ASM_OP "\t.csect .text[PR],5"

/* Output before writable data.  */
#define DATA_SECTION_ASM_OP \
  "\t.csect .data[RW]," XCOFF_CSECT_DEFAULT_ALIGNMENT_STR


/* The eh_frames are put in the read-only text segment.
   Local code labels/function will also be in the local text segment so use
   PC relative addressing.
   Global symbols must be in the data segment to allow loader relocations.
   So use DW_EH_PE_indirect to allocate a slot in the local data segment.
   There is no constant offset to this data segment from the text segment,
   so use addressing relative to the data segment.
 */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL) \
  (((GLOBAL) ? DW_EH_PE_indirect | DW_EH_PE_datarel : DW_EH_PE_pcrel) \
   | (TARGET_64BIT ? DW_EH_PE_sdata8 : DW_EH_PE_sdata4))

#define EH_FRAME_THROUGH_COLLECT2 1
#define EH_TABLES_CAN_BE_READ_ONLY 1

/* AIX Assembler implicitly assumes DWARF 64 bit extension in 64 bit mode.  */
#define DWARF_OFFSET_SIZE PTR_SIZE

#define ASM_OUTPUT_DWARF_PCREL(FILE,SIZE,LABEL) \
  rs6000_asm_output_dwarf_pcrel ((FILE), (SIZE), (LABEL));

#define ASM_OUTPUT_DWARF_DATAREL(FILE,SIZE,LABEL) \
  rs6000_asm_output_dwarf_datarel ((FILE), (SIZE), (LABEL));

#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

