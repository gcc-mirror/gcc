/* Configuration for an i386 running MS-DOS with DJGPP.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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


#include "dbxcoff.h"

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

#define HANDLE_SYSV_PRAGMA

/* Enable parsing of #pragma pack(push,<n>) and #pragma pack(pop).  */
#define HANDLE_PRAGMA_PACK_PUSH_POP 1

#define YES_UNDERSCORES

#include "i386/gas.h"

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP "\t.section\t.bss"

/* Define the name of the .ctor section.  */
#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP "\t.section .ctor"

/* Define the name of the .data section.  */
#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP "\t.section .data"

/* Define the name of the .dtor section.  */
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP "\t.section .dtor"

/* Define the name of the .eh_frame section.  */
#undef EH_FRAME_SECTION_ASM_OP
#define EH_FRAME_SECTION_ASM_OP "\t.section .eh_frame"

/* Define the name of the .ident op.  */
#undef IDENT_ASM_OP
#define IDENT_ASM_OP "\t.ident"

/* Enable alias attribute support.  */
#ifndef SET_ASM_OP
#define SET_ASM_OP "\t.set"
#endif

/* Define the name of the .text section.  */
#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP "\t.section .text"

/* Search for as.exe and ld.exe in DJGPP's binary directory. */ 
#define MD_EXEC_PREFIX "$DJDIR/bin/"

/* Correctly handle absolute filename detection in cp/xref.c */
#define FILE_NAME_ABSOLUTE_P(NAME) \
        (((NAME)[0] == '/') || ((NAME)[0] == '\\') || \
        (((NAME)[0] >= 'A') && ((NAME)[0] <= 'z') && ((NAME)[1] == ':')))

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -DGO32 -DDJGPP=2 -DMSDOS \
  -Asystem(unix) -Asystem(msdos)"

/* Include <sys/version.h> so __DJGPP__ and __DJGPP_MINOR__ are defined.  */
#undef CPP_SPEC
#define CPP_SPEC "-remap %(cpp_cpu) %{posix:-D_POSIX_SOURCE} \
  -imacros %s../include/sys/version.h"
+
/* We need to override link_command_spec in gcc.c so support -Tdjgpp.djl.
   This cannot be done in LINK_SPECS as that LINK_SPECS is processed
   before library search directories are known by the linker.
   This avoids problems when specs file is not available. An alternate way,
   suggested by Robert Hoehne, is to use SUBTARGET_EXTRA_SPECS instead.
*/ 

#undef LINK_COMMAND_SPEC
#define LINK_COMMAND_SPEC \
"%{!fsyntax-only: \
%{!c:%{!M:%{!MM:%{!E:%{!S:%(linker) %l %X %{o*} %{A} %{d} %{e*} %{m} %{N} %{n} \
\t%{r} %{s} %{t} %{u*} %{x} %{z} %{Z}\
\t%{!A:%{!nostdlib:%{!nostartfiles:%S}}}\
\t%{static:} %{L*} %D %o\
\t%{!nostdlib:%{!nodefaultlibs:%G %L %G}}\
\t%{!A:%{!nostdlib:%{!nostartfiles:%E}}}\
\t-Tdjgpp.djl %{T*}}}}}}}\n\
%{!c:%{!M:%{!MM:%{!E:%{!S:stubify %{v} %{o*:%*} %{!o*:a.out} }}}}}"

/* Always just link in 'libc.a'.  */
#undef LIB_SPEC
#define LIB_SPEC "-lc"

/* Pick the right startup code depending on the -pg flag.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:crt0.o%s}"

/* Make sure that gcc will not look for .h files in /usr/local/include 
   unless user explicitly requests it.  */
#undef LOCAL_INCLUDE_DIR

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_ctor, in_dtor

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
  CTOR_SECTION_FUNCTION						\
  DTOR_SECTION_FUNCTION

#define CTOR_SECTION_FUNCTION					\
void								\
ctor_section ()							\
{								\
  if (in_section != in_ctor)					\
    {								\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctor;					\
    }								\
}

#define DTOR_SECTION_FUNCTION					\
void								\
dtor_section ()							\
{								\
  if (in_section != in_dtor)					\
    {								\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtor;					\
    }								\
}

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do {						\
    ctor_section ();				\
    fprintf (FILE, "%s\t", ASM_LONG);		\
    assemble_name (FILE, NAME);			\
    fprintf (FILE, "\n");			\
  } while (0)

/* Allow (eg) __attribute__((section "locked")) to work */
#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC)\
  do {						\
    fprintf (FILE, "\t.section %s\n", NAME);	\
  } while (0)

#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)	\
  do {						\
    dtor_section ();                   		\
    fprintf (FILE, "%s\t", ASM_LONG);		\
    assemble_name (FILE, NAME);              	\
    fprintf (FILE, "\n");			\
  } while (0)

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
  do {									\
	output_file_directive (FILE, main_input_filename);		\
  } while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG) != 0) fprintf ((FILE), "\t.p2align %d\n", LOG)

/* This is how to output a global symbol in the BSS section.  */
#undef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* djgpp automatically calls its own version of __main, so don't define one
   in libgcc, nor call one in main().  */
#define HAS_INIT_SECTION

/* Definitions to set wchar_t type to 'unsigned short int' to help out
   add-on Windows compilers for DJGPP. */
#define WCHAR_UNSIGNED 1
#define WCHAR_TYPE_SIZE 16
#define WCHAR_TYPE "short unsigned int"

/* Used to be defined in xm-djgpp.h, but moved here for cross-compilers.  */
#define LIBSTDCXX "-lstdcxx"
