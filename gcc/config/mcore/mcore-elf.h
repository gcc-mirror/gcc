/* Definitions of MCore target. 
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

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

#ifndef __MCORE_ELF_H__
#define __MCORE_ELF_H__

/* Run-time Target Specification.  */
#define TARGET_VERSION fputs (" (Motorola MCORE/elf)", stderr)

#define SUBTARGET_CPP_PREDEFINES " -D__ELF__"

/* Use DWARF2 debugging info.  */
#define DWARF2_DEBUGGING_INFO 1

#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* But allow DWARF 1 if the user wants it.  */
#define DWARF_DEBUGGING_INFO 1

#define EXPORTS_SECTION_ASM_OP	"\t.section .exports"

#define SUBTARGET_EXTRA_SECTIONS in_exports

#define SUBTARGET_EXTRA_SECTION_FUNCTIONS	\
  EXPORT_SECTION_FUNCTION

#define EXPORT_SECTION_FUNCTION 				\
void								\
exports_section ()						\
{								\
  if (in_section != in_exports)					\
    {								\
      fprintf (asm_out_file, "%s\n", EXPORTS_SECTION_ASM_OP);	\
      in_section = in_exports;					\
    }								\
}

#define SUBTARGET_SWITCH_SECTIONS		\
  case in_exports: exports_section (); break;


#define MCORE_EXPORT_NAME(STREAM, NAME)			\
  do							\
    {							\
      exports_section ();				\
      fprintf (STREAM, "\t.ascii \" -export:%s\"\n",	\
	       (* targetm.strip_name_encoding) (NAME));	\
    }							\
  while (0);

/* Write the extra assembler code needed to declare a function properly.
   Some svr4 assemblers need to also have something extra said about the
   function's return value.  We allow for that here.  */
#undef  ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      if (mcore_dllexport_name_p (NAME))			\
	{							\
          MCORE_EXPORT_NAME (FILE, NAME);			\
	  function_section (DECL);				\
	}							\
      ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "function");	\
      ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));		\
      ASM_OUTPUT_LABEL (FILE, NAME);				\
    }								\
  while (0)

/* Write the extra assembler code needed to declare an object properly.  */
#undef  ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      HOST_WIDE_INT size;					\
      if (mcore_dllexport_name_p (NAME))			\
        {							\
          enum in_section save_section = in_section;		\
	  MCORE_EXPORT_NAME (FILE, NAME);			\
          switch_to_section (save_section, (DECL));		\
        }							\
      ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");		\
      size_directive_output = 0;				\
      if (!flag_inhibit_size_directive && DECL_SIZE (DECL))	\
        {							\
          size_directive_output = 1;				\
	  size = int_size_in_bytes (TREE_TYPE (DECL));		\
	  ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, size);		\
        }							\
      ASM_OUTPUT_LABEL(FILE, NAME);				\
    }								\
  while (0)
 
/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */
#undef  ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)         \
  do                                                                     \
    {                                                                    \
      const char * name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);           \
      HOST_WIDE_INT size;						 \
      if (!flag_inhibit_size_directive && DECL_SIZE (DECL)               \
          && ! AT_END && TOP_LEVEL                                       \
          && DECL_INITIAL (DECL) == error_mark_node                      \
          && !size_directive_output)                                     \
        {                                                                \
	  size_directive_output = 1;					 \
	  size = int_size_in_bytes (TREE_TYPE (DECL));			 \
	  ASM_OUTPUT_SIZE_DIRECTIVE (FILE, name, size);			 \
        }                                                                \
    }                                                                    \
  while (0)


#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crti.o%s crtbegin.o%s"

/* Include the OS stub library, so that the code can be simulated.
   This is not the right way to do this.  Ideally this kind of thing
   should be done in the linker script - but I have not worked out how
   to specify the location of a linker script in a gcc command line yet.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC  "%{!mno-lsim:-lsim} crtend.o%s crtn.o%s"

/* The subroutine calls in the .init and .fini sections create literal
   pools which must be jumped around...  */
#define FORCE_CODE_SECTION_ALIGN	asm ("br 1f ; .literals ; 1:");

#undef  CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors,\"aw\""
#undef  DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors,\"aw\""
     
#endif /* __MCORE_ELF_H__ */
