/* Definitions of target machine for GNU compiler,
   for ARM with ConiX OS.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Philip Blundell <pb@futuretv.com>
   
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

/* Run-time Target Specification.  */
#ifndef TARGET_VERSION
#define TARGET_VERSION	fputs (" (ARM/ELF ConiX)", stderr);
#endif

/* Default to using APCS-32 and software floating point.  */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT	(ARM_FLAG_SOFT_FLOAT | ARM_FLAG_APCS_32)
#endif

/* Now we define the strings used to build the spec file.  */
#define STARTFILE_SPEC		"crtbegin%O%s crt0%O%s"

#define ENDFILE_SPEC		"crtend%O%s"

#define USER_LABEL_PREFIX 	""
#define LOCAL_LABEL_PREFIX 	"."

#define TEXT_SECTION		"\t.text"

/* Debugging */
#define DWARF2_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Support for Constructors and Destructors.  */
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

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  default_elf_asm_named_section

/* The ARM development system defines __main.  */
#define NAME__MAIN "__gccmain"
#define SYMBOL__MAIN __gccmain

#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)
#define UNIQUE_SECTION(DECL,RELOC)				\
  do								\
    {								\
      int len;							\
      char * name, * string, * prefix;				\
      								\
      name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));	\
      								\
      if (! DECL_ONE_ONLY (DECL))				\
	{							\
	  prefix = ".";                                         \
	  if (TREE_CODE (DECL) == FUNCTION_DECL)		\
	    prefix = ".text.";					\
	  else if (DECL_READONLY_SECTION (DECL, RELOC))		\
	    prefix = ".rodata.";				\
	  else							\
	    prefix = ".data.";					\
	}							\
      else if (TREE_CODE (DECL) == FUNCTION_DECL)		\
	prefix = ".gnu.linkonce.t.";				\
      else if (DECL_READONLY_SECTION (DECL, RELOC))		\
	prefix = ".gnu.linkonce.r.";				\
      else							\
	prefix = ".gnu.linkonce.d.";				\
      								\
      len = strlen (name) + strlen (prefix);			\
      string = alloca (len + 1);				\
      sprintf (string, "%s%s", prefix, name);			\
      								\
      DECL_SECTION_NAME (DECL) = build_string (len, string);	\
    }								\
  while (0)

#ifndef CPP_APCS_PC_DEFAULT_SPEC
#define CPP_APCS_PC_DEFAULT_SPEC	"-D__APCS_32__"
#endif
     
#ifndef SUBTARGET_CPU_DEFAULT
#define SUBTARGET_CPU_DEFAULT 		TARGET_CPU_arm7tdmi
#endif
     
#define CPP_PREDEFINES \
  "-D__arm__ -D__CONIX__ -Acpu=arm -Amachine=arm -D__ELF__"

/* Now get the routine arm-elf definitions.  */
#include "arm/elf.h"
