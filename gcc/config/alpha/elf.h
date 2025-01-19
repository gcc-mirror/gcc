/* Definitions of target machine for GNU compiler, for DEC Alpha w/ELF.
   Copyright (C) 1996-2025 Free Software Foundation, Inc.
   Contributed by Richard Henderson (rth@tamu.edu).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#undef  CC1_SPEC
#define CC1_SPEC  "%{G*}"

#undef  ASM_SPEC
#define ASM_SPEC  "%{G*} %{relax:-relax} %{mcpu=*:-m%*}"

/* Do not output a .file directive at the beginning of the input file.  */

#undef  TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE false

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)		\
  if ((LOG) != 0)				\
    fprintf (FILE, "\t.align %d\n", LOG);

/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#undef  ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  if ((SIZE) <= (unsigned HOST_WIDE_INT) g_switch_value)		\
    switch_to_section (sbss_section);					\
  else									\
    switch_to_section (bss_section);					\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
  if (!flag_inhibit_size_directive)					\
    ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, SIZE);			\
  ASM_OUTPUT_ALIGN ((FILE), exact_log2((ALIGN) / BITS_PER_UNIT));	\
  ASM_OUTPUT_LABEL(FILE, NAME);						\
  ASM_OUTPUT_SKIP((FILE), (SIZE) ? (SIZE) : 1);				\
} while (0)

/* This says how to output assembler code to declare an
   uninitialized external linkage data object.  */

#undef  ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
do {									\
  ASM_OUTPUT_ALIGNED_LOCAL (FILE, NAME, SIZE, ALIGN);			\
} while (0)

#undef  BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"
#undef  SBSS_SECTION_ASM_OP
#define SBSS_SECTION_ASM_OP	"\t.section\t.sbss,\"aw\""
#undef  SDATA_SECTION_ASM_OP
#define SDATA_SECTION_ASM_OP	"\t.section\t.sdata,\"aw\""

/* This is how we tell the assembler that two symbols have the same value.  */

#undef  ASM_OUTPUT_DEF
#define ASM_OUTPUT_DEF(FILE, ALIAS, NAME)			\
  do {								\
    assemble_name(FILE, ALIAS);					\
    fputs(" = ", FILE);						\
    assemble_name(FILE, NAME);					\
    fputc('\n', FILE);						\
  } while (0)

#undef  ASM_OUTPUT_DEF_FROM_DECLS
#define ASM_OUTPUT_DEF_FROM_DECLS(FILE, DECL, TARGET)		\
  do {								\
    const char *alias = XSTR (XEXP (DECL_RTL (DECL), 0), 0);	\
    const char *name = IDENTIFIER_POINTER (TARGET);		\
    if (TREE_CODE (DECL) == FUNCTION_DECL)			\
      {								\
	fputc ('$', FILE);					\
	assemble_name (FILE, alias);				\
	fputs ("..ng = $", FILE);				\
	assemble_name (FILE, name);				\
	fputs ("..ng\n", FILE);					\
      }								\
    ASM_OUTPUT_DEF (FILE, alias, name);				\
  } while (0)

/* This variable should be set to 'true' if the target ABI requires
   unwinding tables even when exceptions are not used.  */
#define TARGET_UNWIND_TABLES_DEFAULT true

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.

   Since application size is already constrained to <2GB by the form of
   the ldgp relocation, we can use a 32-bit pc-relative relocation to
   static data.  Dynamic data is accessed indirectly to allow for read
   only EH sections.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)       \
  (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4)

/* If defined, a C statement to be executed just prior to the output of
   assembler code for INSN.  */
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)	\
 (alpha_this_literal_sequence_number = 0,		\
  alpha_this_gpdisp_sequence_number = 0)
extern int alpha_this_literal_sequence_number;
extern int alpha_this_gpdisp_sequence_number;

/* Since the bits of the _init and _fini function is spread across
   many object files, each potentially with its own GP, we must assume
   we need to load our GP.  Further, the .init/.fini section can
   easily be more than 4MB away from the function to call so we can't
   use bsr.  */
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
   asm (SECTION_OP "\n"					\
"	br $29,1f\n"					\
"1:	ldgp $29,0($29)\n"				\
"	unop\n"						\
"	jsr $26," USER_LABEL_PREFIX #FUNC "\n"		\
"	.align 3\n"					\
"	.previous");

/* If we have the capability create headers for efficient EH lookup.
   As of Jan 2002, only glibc 2.2.4 can actually make use of this, but
   I imagine that other systems will catch up.  In the meantime, it
   doesn't harm to make sure that the data exists to be used later.  */
#if defined(HAVE_LD_EH_FRAME_HDR)
#define LINK_EH_SPEC "%{!static|static-pie:--eh-frame-hdr} "
#endif
