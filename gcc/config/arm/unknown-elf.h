/* Definitions for non-Linux based ARM systems using ELF
   Copyright (C) 1998 Free Software Foundation, Inc.
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

/* If you don't define HAVE_ATEXIT, and the object file format/OS/whatever 
   does not support constructors/destructors, then gcc implements destructors
   by defining its own exit function, which calls the destructors.  This gcc
   exit function overrides the C library's exit function, and this can cause
   all kinds of havoc if the C library has a non-trivial exit function.  You
   really don't want to use the exit function in libgcc2.c.  */
#define HAVE_ATEXIT

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
#define ASM_OUTPUT_SECTION_NAME(STREAM, DECL, NAME, RELOC) \
do {								\
  if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)		\
    fprintf (STREAM, "\t.section %s,\"ax\",@progbits\n", (NAME)); \
  else if ((DECL) && DECL_READONLY_SECTION (DECL, RELOC))	\
    fprintf (STREAM, "\t.section %s,\"a\"\n", (NAME));		\
  else								\
    fprintf (STREAM, "\t.section %s,\"aw\"\n", (NAME));		\
} while (0)

/* Don't know how to order these.  UNALIGNED_WORD_ASM_OP is in
   dwarf2.out. */ 
#define UNALIGNED_WORD_ASM_OP ".4byte"

#define ASM_OUTPUT_DWARF2_ADDR_CONST(FILE,ADDR)                  \
     fprintf ((FILE), "\t%s\t%s", UNALIGNED_WORD_ASM_OP, ADDR)

#define ASM_OUTPUT_DWARF_ADDR_CONST(FILE,RTX)                   \
do {								\
  fprintf ((FILE), "\t%s\t", UNALIGNED_WORD_ASM_OP);		\
  output_addr_const ((FILE), (RTX));				\
  fputc ('\n', (FILE));						\
} while (0)


/* The ARM development system defines __main.  */
#define NAME__MAIN "__gccmain"
#define SYMBOL__MAIN __gccmain

#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)
#define UNIQUE_SECTION_P(DECL) (DECL_ONE_ONLY (DECL))
#define UNIQUE_SECTION(DECL,RELOC)				\
do {								\
  int len;							\
  char * name, * string, * prefix;				\
								\
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));	\
								\
  if (! DECL_ONE_ONLY (DECL))					\
    {								\
      prefix = ".";                                             \
      if (TREE_CODE (DECL) == FUNCTION_DECL)			\
	prefix = ".text.";					\
      else if (DECL_READONLY_SECTION (DECL, RELOC))		\
	prefix = ".rodata.";					\
      else							\
	prefix = ".data.";					\
    }								\
  else if (TREE_CODE (DECL) == FUNCTION_DECL)			\
    prefix = ".gnu.linkonce.t.";				\
  else if (DECL_READONLY_SECTION (DECL, RELOC))			\
    prefix = ".gnu.linkonce.r.";				\
  else								\
    prefix = ".gnu.linkonce.d.";				\
								\
  len = strlen (name) + strlen (prefix);			\
  string = alloca (len + 1);					\
  sprintf (string, "%s%s", prefix, name);			\
								\
  DECL_SECTION_NAME (DECL) = build_string (len, string);	\
} while (0)

#ifndef CPP_APCS_PC_DEFAULT_SPEC
#define CPP_APCS_PC_DEFAULT_SPEC	"-D__APCS_32__"
#endif
     
#ifndef SUBTARGET_CPU_DEFAULT
#define SUBTARGET_CPU_DEFAULT 		TARGET_CPU_arm7tdmi
#endif
     
/* Now get the routine arm-elf definitions.  */
#include "arm/elf.h"
