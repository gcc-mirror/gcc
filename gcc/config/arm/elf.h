/* Definitions of target machine for GNU compiler.
   For ARM with ELF obj format.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.
   Contributed by Philip Blundell <philb@gnu.org> and
   Catherine Moore <clm@cygnus.com>
   
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


#define OBJECT_FORMAT_ELF

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."
#endif

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""
#endif

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC  "-D__ELF__"
#endif

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "subtarget_extra_asm_spec",	SUBTARGET_EXTRA_ASM_SPEC },
#endif

#ifndef SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC ""
#endif

#ifndef ASM_SPEC
#define ASM_SPEC "\
%{mbig-endian:-EB} \
%{mcpu=*:-m%*} \
%{march=*:-m%*} \
%{mapcs-*:-mapcs-%*} \
%{mapcs-float:-mfloat} \
%{msoft-float:-mno-fpu} \
%{mthumb-interwork:-mthumb-interwork} \
%(subtarget_extra_asm_spec)"
#endif

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */
#define TYPE_OPERAND_FMT	"%s"

/* Write the extra assembler code needed to declare a function's result.
   Most svr4 assemblers don't require any special declaration of the
   result value, but there are exceptions.  */
#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */
#define TYPE_ASM_OP     "\t.type\t"
#define SIZE_ASM_OP     "\t.size\t"

/* Write the extra assembler code needed to declare a function properly.
   Some svr4 assemblers need to also have something extra said about the
   function's return value.  We allow for that here.  */
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)	\
  do							\
    {							\
      ARM_DECLARE_FUNCTION_NAME (FILE, NAME, DECL);     \
      fprintf (FILE, "%s", TYPE_ASM_OP);		\
      assemble_name (FILE, NAME);			\
      putc (',', FILE);					\
      fprintf (FILE, TYPE_OPERAND_FMT, "function");	\
      putc ('\n', FILE);				\
      ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));	\
      ASM_OUTPUT_LABEL(FILE, NAME);			\
    }							\
  while (0)

/* Write the extra assembler code needed to declare an object properly.  */
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      fprintf (FILE, "%s", TYPE_ASM_OP);			\
      assemble_name (FILE, NAME);				\
      putc (',', FILE);						\
      fprintf (FILE, TYPE_OPERAND_FMT, "object");		\
      putc ('\n', FILE);					\
      size_directive_output = 0;				\
      if (!flag_inhibit_size_directive && DECL_SIZE (DECL))	\
        {							\
	  size_directive_output = 1;				\
	  fprintf (FILE, "%s", SIZE_ASM_OP);			\
	  assemble_name (FILE, NAME);				\
	  putc (',', FILE);					\
	  fprintf (FILE, HOST_WIDE_INT_PRINT_DEC,		\
		   int_size_in_bytes (TREE_TYPE (DECL)));	\
	  fputc ('\n', FILE);					\
        }							\
      ASM_OUTPUT_LABEL(FILE, NAME);				\
    }								\
  while (0)

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	\
  do									\
    {									\
      const char * name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		\
      if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		\
          && ! AT_END && TOP_LEVEL					\
	  && DECL_INITIAL (DECL) == error_mark_node			\
	  && !size_directive_output)					\
        {								\
	  size_directive_output = 1;					\
	  fprintf (FILE, "%s", SIZE_ASM_OP);				\
	  assemble_name (FILE, name);					\
	  putc (',', FILE);						\
	  fprintf (FILE, HOST_WIDE_INT_PRINT_DEC,			\
		  int_size_in_bytes (TREE_TYPE (DECL)));		\
	 fputc ('\n', FILE);						\
        }								\
    }									\
  while (0)

/* This is how to declare the size of a function.  */
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do								\
    {								\
      ARM_DECLARE_FUNCTION_SIZE (FILE, FNAME, DECL);		\
      if (!flag_inhibit_size_directive)				\
        {							\
          char label[256];					\
	  static int labelno;					\
	  labelno ++;						\
	  ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);	\
	  ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);	\
	  fprintf (FILE, "%s", SIZE_ASM_OP);			\
	  assemble_name (FILE, (FNAME));			\
          fprintf (FILE, ",");					\
	  assemble_name (FILE, label);				\
          fprintf (FILE, "-");					\
	  assemble_name (FILE, (FNAME));			\
	  putc ('\n', FILE);					\
        }							\
    }								\
  while (0)

/* Define this macro if jump tables (for `tablejump' insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

#ifndef LINK_SPEC
#define LINK_SPEC "%{mbig-endian:-EB} -X"
#endif
  
/* Run-time Target Specification.  */
#ifndef TARGET_VERSION
#define TARGET_VERSION fputs (" (ARM/elf)", stderr)
#endif

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (ARM_FLAG_SOFT_FLOAT | ARM_FLAG_APCS_32 | ARM_FLAG_APCS_FRAME)
#endif

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
  { "marm", "mlittle-endian", "msoft-float", "mapcs-32", "mno-thumb-interwork", "fno-leading-underscore" }
#endif


/* This outputs a lot of .req's to define alias for various registers.
   Let's try to avoid this.  */
#ifndef ASM_FILE_START
#define ASM_FILE_START(STREAM)					\
  do								\
    {								\
      fprintf (STREAM, "%s Generated by gcc %s for ARM/elf\n",	\
	       ASM_COMMENT_START, version_string);		\
      output_file_directive (STREAM, main_input_filename);	\
      fprintf (STREAM, ASM_APP_OFF);				\
    }								\
  while (0)
#endif

/* Output an internal label definition.  */
#ifndef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(STREAM, PREFIX, NUM)  	\
  do								\
    {								\
      char * s = (char *) alloca (40 + strlen (PREFIX));	\
      extern int arm_target_label, arm_ccfsm_state;		\
      extern rtx arm_target_insn;				\
								\
      if (arm_ccfsm_state == 3 && arm_target_label == (NUM)	\
	&& !strcmp (PREFIX, "L"))				\
	{							\
	  arm_ccfsm_state = 0;					\
	  arm_target_insn = NULL;				\
	}							\
	ASM_GENERATE_INTERNAL_LABEL (s, (PREFIX), (NUM));	\
        ASM_OUTPUT_LABEL (STREAM, s);		                \
    }								\
  while (0)
#endif

/* A list of other sections which the compiler might be "in" at any
   given time.  */
#ifndef SUBTARGET_EXTRA_SECTIONS
#define SUBTARGET_EXTRA_SECTIONS
#endif

#ifndef EXTRA_SECTIONS
#define EXTRA_SECTIONS SUBTARGET_EXTRA_SECTIONS
#endif

/* A list of extra section function definitions.  */
#ifndef SUBTARGET_EXTRA_SECTION_FUNCTIONS
#define SUBTARGET_EXTRA_SECTION_FUNCTIONS
#endif

#ifndef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS 		\
  SUBTARGET_EXTRA_SECTION_FUNCTIONS
#endif

/* Switch into a generic section.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  arm_elf_asm_named_section

/* Support the ctors/dtors sections for g++.  */
#ifndef INT_ASM_OP
#define INT_ASM_OP 	"\t.word\t"
#endif

/* This is how we tell the assembler that a symbol is weak.  */

#define ASM_WEAKEN_LABEL(FILE, NAME)		\
  do						\
    {						\
      fputs ("\t.weak\t", FILE);		\
      assemble_name (FILE, NAME);		\
      fputc ('\n', FILE);			\
    }						\
  while (0)

#ifndef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(STREAM, NAME, SIZE, ALIGN)	\
  do								\
    {								\
      fprintf (STREAM, "\t.comm\t");				\
      assemble_name (STREAM, NAME);				\
      fprintf (STREAM, ", %d, %d\n", SIZE, ALIGN);		\
    }								\
  while (0)
#endif

/* For PIC code we need to explicitly specify (PLT) and (GOT) relocs.  */
#define NEED_PLT_RELOC	flag_pic
#define NEED_GOT_RELOC	flag_pic

/* The ELF assembler handles GOT addressing differently to NetBSD.  */
#define GOT_PCREL	0

/* Biggest alignment supported by the object file format of this
   machine.  Use this macro to limit the alignment which can be
   specified using the `__attribute__ ((aligned (N)))' construct.  If
   not defined, the default value is `BIGGEST_ALIGNMENT'.  */
#define MAX_OFILE_ALIGNMENT (32768 * 8)

/* Align output to a power of two.  Note ".align 0" is redundant,
   and also GAS will treat it as ".align 2" which we do not want.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER)			\
  do							\
    {							\
      if ((POWER) > 0)					\
	fprintf (STREAM, "\t.align\t%d\n", POWER);	\
    }							\
  while (0)

#include "aout.h"
