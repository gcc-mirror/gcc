/* Definitions of target machine for GNU compiler, for ARM with PE obj format.
   Copyright (C) 1995, 1996, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Doug Evans (dje@cygnus.com).
   
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

#include "arm/coff.h"

#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"


/* Run-time Target Specification.  */
#undef  TARGET_VERSION
#define TARGET_VERSION fputs (" (ARM/pe)", stderr)

/* Support the __declspec keyword by turning them into attributes.
   We currently only support: naked, dllimport, and dllexport.
   Note that the current way we do this may result in a collision with
   predefined attributes later on.  This can be solved by using one attribute,
   say __declspec__, and passing args to it.  The problem with that approach
   is that args are not accumulated: each new appearance would clobber any
   existing args.  */
#undef  CPP_PREDEFINES
#define CPP_PREDEFINES "\
-Darm -D__pe__ -Acpu(arm) -Amachine(arm) \
-D__declspec(x)=__attribute__((x)) \
"

/* Experimental addition for pr 7885.
   Ignore dllimport for functions.  */
#define TARGET_FLAG_NOP_FUN	(1 << 24)

#undef  TARGET_NOP_FUN_DLLIMPORT
#define TARGET_NOP_FUN_DLLIMPORT (target_flags & TARGET_FLAG_NOP_FUN)

#undef  SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
{ "nop-fun-dllimport",		  TARGET_FLAG_NOP_FUN, "Ignore dllimport attribute for functions" }, \
{ "no-nop-fun-dllimport",	- TARGET_FLAG_NOP_FUN, "" },

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT	(ARM_FLAG_SOFT_FLOAT | TARGET_FLAG_NOP_FUN)


#undef  WCHAR_TYPE
#define WCHAR_TYPE 	"short unsigned int"
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* Same as arm.h except r10 is call-saved, not fixed.  */
#undef  FIXED_REGISTERS
#define FIXED_REGISTERS \
{			\
  0,0,0,0,0,0,0,0,	\
  0,0,0,1,0,1,0,1,	\
  0,0,0,0,0,0,0,0,	\
  1,1,1			\
}

/* Same as arm.h except r10 is call-saved, not fixed.  */
#undef  CALL_USED_REGISTERS
#define CALL_USED_REGISTERS \
{			\
  1,1,1,1,0,0,0,0,	\
  0,0,0,1,1,1,1,1,	\
  1,1,1,1,0,0,0,0,	\
  1,1,1			\
}

/* This is to better conform to the ARM PCS.
   Richard Earnshaw hasn't put this into FSF sources yet so it's here.  */
#undef  RETURN_IN_MEMORY
#define RETURN_IN_MEMORY(TYPE) 						\
  ((TYPE_MODE ((TYPE)) == BLKmode && ! TYPE_NO_FORCE_BLK (TYPE))	\
   || (AGGREGATE_TYPE_P ((TYPE)) && arm_pe_return_in_memory ((TYPE))))

/* A C expression whose value is nonzero if IDENTIFIER with arguments ARGS
   is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to DECL.  */
#undef  VALID_MACHINE_DECL_ATTRIBUTE
#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, IDENTIFIER, ARGS) \
  arm_pe_valid_machine_decl_attribute (DECL, ATTRIBUTES, IDENTIFIER, ARGS)

#define MERGE_MACHINE_DECL_ATTRIBUTES(OLD, NEW) \
  arm_pe_merge_machine_decl_attributes ((OLD), (NEW))

/* In addition to the stuff done in arm.h, we must mark dll symbols specially.
   Definitions of dllexport'd objects install some info in the .drectve
   section.  References to dllimport'd objects are fetched indirectly via
   __imp_.  If both are declared, dllexport overrides.
   This is also needed to implement one-only vtables: they go into their own
   section and we need to set DECL_SECTION_NAME so we do that here.
   Note that we can be called twice on the same decl.  */
#undef  ENCODE_SECTION_INFO
#define ENCODE_SECTION_INFO(DECL) \
  arm_pe_encode_section_info (DECL)

/* Used to implement dllexport overriding dllimport semantics.  It's also used
   to handle vtables - the first pass won't do anything because
   DECL_CONTEXT (DECL) will be 0 so arm_dll{ex,im}port_p will return 0.
   It's also used to handle dllimport override semantics.  */
#if 0
#define REDO_SECTION_INFO_P(DECL) \
((DECL_MACHINE_ATTRIBUTES (DECL) != NULL_TREE) \
 || (TREE_CODE (DECL) == VAR_DECL && DECL_VIRTUAL_P (DECL)))
#else
#define REDO_SECTION_INFO_P(DECL) 1
#endif

/* Utility used only in this file.  */
#define ARM_STRIP_NAME_ENCODING(SYM_NAME) \
((SYM_NAME) + ((SYM_NAME)[0] == '@' ? 3 : 0))

/* Strip any text from SYM_NAME added by ENCODE_SECTION_INFO and store
   the result in VAR.  */
#undef  STRIP_NAME_ENCODING
#define STRIP_NAME_ENCODING(VAR, SYM_NAME) \
(VAR) = ARM_STRIP_NAME_ENCODING (SYM_NAME)

/* Define this macro if in some cases global symbols from one translation
   unit may not be bound to undefined symbols in another translation unit
   without user intervention.  For instance, under Microsoft Windows
   symbols must be explicitly imported from shared libraries (DLLs).  */
#define MULTIPLE_SYMBOL_SPACES

#define UNIQUE_SECTION_P(DECL) DECL_ONE_ONLY (DECL)

#define UNIQUE_SECTION(DECL, RELOC) arm_pe_unique_section (DECL, RELOC)

#define SUPPORTS_ONE_ONLY 1

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL or
   NULL_TREE.  Some target formats do not support arbitrary sections.  Do not
   define this macro in such cases.  */
#undef  ASM_OUTPUT_SECTION_NAME
#define ASM_OUTPUT_SECTION_NAME(STREAM, DECL, NAME, RELOC) 	\
  do								\
    {								\
      if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)		\
	fprintf (STREAM, "\t.section %s,\"x\"\n", (NAME));	\
      else if ((DECL) && DECL_READONLY_SECTION (DECL, RELOC))	\
	fprintf (STREAM, "\t.section %s,\"\"\n", (NAME));	\
      else							\
	fprintf (STREAM, "\t.section %s,\"w\"\n", (NAME));	\
      /* Functions may have been compiled at various levels of	\
	 optimization so we can't use `same_size' here.		\
	 Instead, have the linker pick one.  */			\
      if ((DECL) && DECL_ONE_ONLY (DECL))			\
	fprintf (STREAM, "\t.linkonce %s\n",			\
		 TREE_CODE (DECL) == FUNCTION_DECL		\
		 ? "discard" : "same_size");			\
    }								\
  while (0)

/* This outputs a lot of .req's to define alias for various registers.
   Let's try to avoid this.  */
#undef  ASM_FILE_START
#define ASM_FILE_START(STREAM)					\
  do								\
    {								\
      extern char * version_string;				\
      asm_fprintf (STREAM, "%@ Generated by gcc %s for ARM/pe\n",\
	   version_string);					\
      output_file_directive ((STREAM), main_input_filename);	\
    }								\
  while (0)

/* Output a reference to a label.  */
#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME)  \
  fprintf (STREAM, "%s%s", USER_LABEL_PREFIX, ARM_STRIP_NAME_ENCODING (NAME))

/* Output a function definition label.  */
#undef  ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)   \
  do							\
    {							\
      if (arm_dllexport_name_p (NAME))			\
	{						\
	  drectve_section ();				\
	  fprintf (STREAM, "\t.ascii \" -export:%s\"\n",\
		   ARM_STRIP_NAME_ENCODING (NAME));	\
	  function_section (DECL);			\
	}						\
      if (TARGET_POKE_FUNCTION_NAME)			\
	arm_poke_function_name ((STREAM), (NAME));	\
      ASM_OUTPUT_LABEL ((STREAM), (NAME));		\
    }							\
  while (0)

/* Output a common block.  */
#undef  ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)	\
  do							\
    {							\
      if (arm_dllexport_name_p (NAME))			\
	{						\
	  drectve_section ();				\
	  fprintf ((STREAM), "\t.ascii \" -export:%s\"\n",\
		   ARM_STRIP_NAME_ENCODING (NAME));	\
	}						\
      if (! arm_dllimport_name_p (NAME))		\
	{						\
	  fprintf ((STREAM), "\t.comm\t"); 		\
	  assemble_name ((STREAM), (NAME));		\
	  asm_fprintf ((STREAM), ", %d\t%@ %d\n",	\
		   (ROUNDED), (SIZE));			\
	}						\
    }							\
  while (0)

/* Output the label for an initialized variable.  */
#undef  ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL) 	\
  do							\
    {							\
      if (arm_dllexport_name_p (NAME))			\
	{						\
	  enum in_section save_section = in_section;	\
	  drectve_section ();				\
	  fprintf (STREAM, "\t.ascii \" -export:%s\"\n",\
		   ARM_STRIP_NAME_ENCODING (NAME));	\
	  switch_to_section (save_section, (DECL));	\
	}						\
      ASM_OUTPUT_LABEL ((STREAM), (NAME));		\
    }							\
  while (0)

/* Support the ctors/dtors and other sections.  */

#define DRECTVE_SECTION_ASM_OP	"\t.section .drectve"

/* A list of other sections which the compiler might be "in" at any
   given time.  */

#undef  SUBTARGET_EXTRA_SECTIONS
#define SUBTARGET_EXTRA_SECTIONS in_drectve,

/* A list of extra section function definitions.  */

#undef  SUBTARGET_EXTRA_SECTION_FUNCTIONS
#define SUBTARGET_EXTRA_SECTION_FUNCTIONS \
  DRECTVE_SECTION_FUNCTION	\
  SWITCH_TO_SECTION_FUNCTION

#define DRECTVE_SECTION_FUNCTION \
void									\
drectve_section ()							\
{									\
  if (in_section != in_drectve)						\
    {									\
      fprintf (asm_out_file, "%s\n", DRECTVE_SECTION_ASM_OP);		\
      in_section = in_drectve;						\
    }									\
}

/* Switch to SECTION (an `enum in_section').

   ??? This facility should be provided by GCC proper.
   The problem is that we want to temporarily switch sections in
   ASM_DECLARE_OBJECT_NAME and then switch back to the original section
   afterwards.  */
#define SWITCH_TO_SECTION_FUNCTION				\
void								\
switch_to_section (section, decl)				\
     enum in_section section;					\
     tree decl;							\
{								\
  switch (section)						\
    {								\
      case in_text: text_section (); break;			\
      case in_data: data_section (); break;			\
      case in_named: named_section (decl, NULL, 0); break;	\
      case in_rdata: rdata_section (); break;			\
      case in_ctors: ctors_section (); break;			\
      case in_dtors: dtors_section (); break;			\
      case in_drectve: drectve_section (); break;		\
      default: abort (); break;					\
    }								\
}
