/* Definitions for non-Linux based ARM systems using ELF
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Catherine Moore <clm@cygnus.com>

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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Run-time Target Specification.  */
#ifndef TARGET_VERSION
#define TARGET_VERSION	fputs (" (ARM/ELF non-Linux)", stderr);
#endif

/* Default to using APCS-32 and software floating point.  */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT	(ARM_FLAG_SOFT_FLOAT | ARM_FLAG_APCS_32)
#endif

/* Now we define the strings used to build the spec file.  */
#define STARTFILE_SPEC	"crtbegin%O%s crt0%O%s"

#define ENDFILE_SPEC	"crtend%O%s"

#define USER_LABEL_PREFIX 	""
#define LOCAL_LABEL_PREFIX 	"."

#define TEXT_SECTION "		.text"

#define INVOKE__main

/* Debugging */
#define DWARF_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Support for Constructors and Destrcutors .  */
#define READONLY_DATA_SECTION	rdata_section

/* A list of other sections which the compiler might be "in" at any
   given time.  */
#define SUBTARGET_EXTRA_SECTIONS in_rdata,

/* A list of extra section function definitions.  */
#define SUBTARGET_EXTRA_SECTION_FUNCTIONS	RDATA_SECTION_FUNCTION
  
#define RDATA_SECTION_ASM_OP	"\t.section .rodata"

#define RDATA_SECTION_FUNCTION 					\
void								\
rdata_section ()						\
{								\
  if (in_section != in_rdata)					\
    {								\
      fprintf (asm_out_file, "%s\n", RDATA_SECTION_ASM_OP);	\
      in_section = in_rdata;					\
    }								\
}

#define CTOR_LIST_BEGIN					\
asm (CTORS_SECTION_ASM_OP);				\
func_ptr __CTOR_LIST__[1] = { (func_ptr) (-1) }

#define CTOR_LIST_END					\
asm (CTORS_SECTION_ASM_OP);				\
func_ptr __CTOR_END__[1] = { (func_ptr) 0 };

#define DTOR_LIST_BEGIN					\
asm (DTORS_SECTION_ASM_OP);				\
func_ptr __DTOR_LIST__[1] = { (func_ptr) (-1) }

#define DTOR_LIST_END					\
asm (DTORS_SECTION_ASM_OP);				\
func_ptr __DTOR_END__[1] = { (func_ptr) 0 };

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL or
   NULL_TREE.  Some target formats do not support arbitrary sections.  Do not
   define this macro in such cases.  */
#define ASM_OUTPUT_SECTION_NAME(STREAM, DECL, NAME, RELOC)        	\
  do									\
    {								  	\
      if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)		  	\
	fprintf (STREAM, "\t.section %s,\"ax\",%%progbits\n", NAME);	\
      else if ((DECL) && DECL_READONLY_SECTION (DECL, RELOC))	  	\
	fprintf (STREAM, "\t.section %s,\"a\"\n", NAME);		\
      else if (! strncmp (NAME, ".bss", 4))      			\
	fprintf (STREAM, "\t.section %s,\"aw\",%%nobits\n", NAME);	\
      else							 	\
	fprintf (STREAM, "\t.section %s,\"aw\"\n", NAME);	  	\
    }									\
  while (0)

/* Don't know how to order these.  UNALIGNED_WORD_ASM_OP is in
   dwarf2.out. */ 
#define UNALIGNED_WORD_ASM_OP ".4byte"

#define ASM_OUTPUT_DWARF_ADDR_CONST(FILE,RTX)                   \
do {								\
  fprintf ((FILE), "\t%s\t", UNALIGNED_WORD_ASM_OP);		\
  output_addr_const ((FILE), (RTX));				\
  fputc ('\n', (FILE));						\
} while (0)


/* The ARM development system defines __main.  */
#define NAME__MAIN "__gccmain"
#define SYMBOL__MAIN __gccmain

/* Return a non-zero value if DECL has a section attribute.  */
#define IN_NAMED_SECTION(DECL)						\
  ((TREE_CODE (DECL) == FUNCTION_DECL || TREE_CODE (DECL) == VAR_DECL)	\
   && DECL_SECTION_NAME (DECL) != NULL_TREE)


#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)
     
#define UNIQUE_SECTION_P(DECL) (DECL_ONE_ONLY (DECL) || flag_data_sections)
     
#define UNIQUE_SECTION(DECL, RELOC)					\
  do									\
    {									\
      int len;								\
      int sec;								\
      char *name;							\
      char *string;							\
      char *prefix;							\
      static char *prefixes[4][2] =					\
      {									\
	{ ".text.",   ".gnu.linkonce.t." },				\
	{ ".rodata.", ".gnu.linkonce.r." },				\
	{ ".data.",   ".gnu.linkonce.d." },				\
        { ".bss.",    ".gnu.linkonce.b." }				\
      };								\
      									\
      if (TREE_CODE (DECL) == FUNCTION_DECL)				\
	sec = 0;							\
      else if (DECL_INITIAL (DECL) == 0					\
	       || DECL_INITIAL (DECL) == error_mark_node)		\
	sec = 3;							\
      else if (DECL_READONLY_SECTION (DECL, RELOC))			\
	sec = 1;							\
      else								\
	sec = 2;							\
      									\
      prefix = prefixes[sec][DECL_ONE_ONLY(DECL)];			\
      name   = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));		\
      									\
      /* Strip off any encoding in name.  */				\
      STRIP_NAME_ENCODING (name, name);					\
									\
      len    = strlen (name) + strlen (prefix);				\
      string = alloca (len + 1);					\
      									\
      sprintf (string, "%s%s", prefix, name);				\
      									\
      DECL_SECTION_NAME (DECL) = build_string (len, string);		\
    }									\
  while (0)

#undef  ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)   	\
  do									\
    {									\
      if (IN_NAMED_SECTION (DECL))					\
	named_section (DECL, NULL, 0);					\
      else								\
	bss_section ();							\
      									\
      ASM_GLOBALIZE_LABEL (FILE, NAME);					\
      									\
      ASM_OUTPUT_ALIGN (FILE, floor_log2 (ALIGN / BITS_PER_UNIT));	\
									\
      last_assemble_variable_decl = DECL;				\
      ASM_DECLARE_OBJECT_NAME (FILE, NAME, DECL);			\
      ASM_OUTPUT_SKIP (FILE, SIZE ? SIZE : 1);				\
    } 									\
  while (0)

#undef  ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
  do									\
    {									\
      if (IN_NAMED_SECTION (DECL))					\
	named_section (DECL, NULL, 0);					\
      else								\
	bss_section ();							\
									\
      ASM_OUTPUT_ALIGN (FILE, floor_log2 (ALIGN / BITS_PER_UNIT));	\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
      fprintf (FILE, "\t.space\t%d\n", SIZE);				\
    }									\
  while (0)

#ifndef CPP_APCS_PC_DEFAULT_SPEC
#define CPP_APCS_PC_DEFAULT_SPEC	"-D__APCS_32__"
#endif
     
#ifndef SUBTARGET_CPU_DEFAULT
#define SUBTARGET_CPU_DEFAULT 		TARGET_CPU_arm7tdmi
#endif
     
/* Now get the routine arm-elf definitions.  */
#include "arm/elf.h"
