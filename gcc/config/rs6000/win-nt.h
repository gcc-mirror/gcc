/* Definitions of target machine for GNU compiler, for PowerPC
   running Windows/NT.
   Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

/* Say this is Windows/NT for the other config files.  */
#define WINDOWS_NT 1
#define COFF_WITH_PE 1

/* Default ABI to compile code for */
#define DEFAULT_ABI ABI_NT

#define CPP_DEFAULT_SPEC "-D_ARCH_PPC"

#define ASM_DEFAULT_SPEC "-mppc"

/* Pseudo target that we can test in the md file.  */
#define	TARGET_WINDOWS_NT 1

#include "rs6000/rs6000.h"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS)

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_POWERPC

#undef	CPP_PREDEFINES
#define	CPP_PREDEFINES "-DWIN32 -D_WIN32 \
  -DWINNT -D__STDC__=0 -DALMOST_STDC \
  -D_POWER -D_ARCH_PPC -D__PPC__ -Asystem(winnt) -Acpu(powerpc) -Amachine(powerpc)"

#if 0
#include "winnt/win-nt.h"
#endif

#undef	LIB_SPEC
#define	LIB_SPEC "%{mwindows:-subsystem:windows -entry:WinMainCRTStartup \
  USER32.LIB GDI32.LIB COMDLG32.LIB WINSPOOL.LIB} \
 %{!mwindows:-subsystem console -e mainCRTStartup} \
 %{mcrtmt:LIBCMT.LIB KERNEL32.LIB} %{!mcrtmt:-lkernel32 -lcygwin} \
 %{v}"

#undef	LINK_SPEC
#define	LINK_SPEC "%{v:-V}"

/* Allow switches specified in LIB_SPEC, but don't do anything with them
   in the compiler.  */
#undef	SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES	\
   { "windows",	0 },		\
   { "crtmt",	0 },

#undef XCOFF_DEBUGGING_INFO

/* this is pure coff, not xcoff */
#define SDB_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

#undef  SDB_DELIM
#define SDB_DELIM ";"

#undef	PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* NT always runs little endian */
#undef  BYTES_BIG_ENDIAN
#define BYTES_BIG_ENDIAN 0 

#undef  WORDS_BIG_ENDIAN
#define WORDS_BIG_ENDIAN 0 

/* Define cutoff for using external functions to save floating point.
   Currently on NT, always use inline stores */
#undef	FP_SAVE_INLINE
#define FP_SAVE_INLINE(FIRST_REG) ((FIRST_REG) < 64)

/* Note, little endian systems trap on unaligned addresses, so never
   turn off strict alignment in that case. */

#undef STRICT_ALIGNMENT
#define	STRICT_ALIGNMENT 1

/* Align stack to 16 byte boundaries */
#undef	STACK_BOUNDARY
#define	STACK_BOUNDARY	128

/* No data type wants to be aligned rounder than this.  */
#undef	BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 128

/* NT aligns internal doubles in structures on dword boundaries.  */
#undef	BIGGEST_FIELD_ALIGNMENT
#define BIGGEST_FIELD_ALIGNMENT 64

#undef  ADJUST_FIELD_ALIGN
#undef  ROUND_TYPE_ALIGN

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC PE)");

#undef TARGET_DEFAULT 
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS | MASK_NO_FP_IN_TOC | MASK_NO_SUM_IN_TOC)

/* MEM representing address to save the TOC register */
#undef	RS6000_SAVE_TOC
#define RS6000_SAVE_TOC gen_rtx_MEM (Pmode, \
				     plus_constant (virtual_incoming_args_rtx,
						    -RS6000_SAVE_AREA - 8))

/* Windows NT specifies that r13 is reserved to the OS, so it is not available
   to the normal user.  */

#undef	FIXED_R13
#define FIXED_R13 1

/* This says how to output an assembler line
   to define a global common symbol.  */

#undef	ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGNMENT)	\
  do { fputs ("\t.comm \t", (FILE));			        \
       assemble_name ((FILE), (NAME));				\
       if ( (SIZE) > 4)                                         \
         fprintf ((FILE), ",%d,%d\n", (SIZE), 3);               \
       else                                                     \
	 fprintf( (FILE), ",%d\n", (SIZE));                     \
  } while (0) 

#undef	ASM_OUTPUT_ALIGNED_LOCAL

/* This says how to output an assembler line
   to define a global common symbol.  */

#undef  ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
  do { fputs ("\t.comm \t", (FILE));			\
       assemble_name ((FILE), (NAME));			\
       fprintf ((FILE), ",%d\n", (SIZE)); } while (0)

/* This says how to output an assembler line
   to define an aligned local common symbol.  */

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  bss_section ();							\
  ASM_OUTPUT_ALIGN (FILE, exact_log2 (ALIGN / BITS_PER_UNIT));		\
  ASM_OUTPUT_LABEL (FILE, NAME);					\
  ASM_OUTPUT_SKIP (FILE, SIZE);						\
} while (0)

/* Describe how to emit uninitialized external linkage items  */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
do {									\
  ASM_GLOBALIZE_LABEL (FILE, NAME);					\
  ASM_OUTPUT_ALIGNED_LOCAL (FILE, NAME, SIZE, ALIGN);			\
} while (0)

/* This says out to put a global symbol in the BSS section */
#undef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))


/* Stuff to force fit us into the Motorola PPC assembler */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
do {									\
  output_file_directive ((FILE), main_input_filename);			\
  rs6000_file_start (FILE, TARGET_CPU_DEFAULT);				\
  data_section ();							\
} while (0)

#undef ASM_FILE_END

#undef	ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE,NAME,DECL)			\
{									\
  tree exception_args;							\
  int i;								\
									\
  if (TREE_PUBLIC (DECL))						\
    {									\
      fprintf (FILE, "\t.globl ..");					\
      assemble_name (FILE, NAME);					\
      fprintf (FILE, "\n");						\
    }									\
									\
  fprintf (FILE, "\n#\tFunction: '..");					\
  assemble_name (FILE, NAME);						\
  fputs ("'\n", FILE);							\
  fputs ("#\tText in section: <default>\n\n", FILE);			\
  fputs ("#\tSetup MS Structured-Exception-Handling\n", FILE);		\
  fputs ("\t.pdata\n", FILE);						\
  fputs ("\t.align 2\n", FILE);						\
  fputs ("\t.ualong ..", FILE);						\
  assemble_name (FILE, NAME);						\
  fputs (",", FILE);							\
  assemble_name (FILE, NAME);						\
  fputs (".e,", FILE);							\
  exception_args = lookup_attribute ("exception",			\
				     TYPE_ATTRIBUTES (TREE_TYPE (DECL))); \
									\
  if (exception_args)							\
    exception_args = TREE_VALUE (exception_args);			\
									\
  for (i = 0; i < 2; i++)						\
    {									\
      if (!exception_args)						\
	fputs ("0,", FILE);						\
      else								\
	{								\
	  tree field = TREE_VALUE (exception_args);			\
	  exception_args = TREE_PURPOSE (exception_args);		\
	  if (TREE_CODE (field) == STRING_CST)				\
	    fprintf (FILE, "%.*s,", TREE_STRING_LENGTH (field),		\
		     TREE_STRING_POINTER (field));			\
	  else if (TREE_CODE (field) == IDENTIFIER_NODE)		\
	    fprintf (FILE, "%.*s,", IDENTIFIER_LENGTH (field),		\
		     IDENTIFIER_POINTER (field));			\
	  else								\
	    abort ();							\
	}								\
    }									\
									\
  assemble_name (FILE, NAME);						\
  fprintf (FILE, ".b\n\n");						\
  fprintf (FILE, "#\tSwitch to the relocation section\n");		\
  fprintf (FILE, "\t.reldata\n");					\
  assemble_name (FILE, NAME);						\
  fprintf (FILE, ":\n");						\
  fprintf (FILE, "\t.ualong ..");					\
  assemble_name (FILE, NAME);						\
  fprintf (FILE, ",.toc\n");						\
									\
  if (lookup_attribute ("dllexport",					\
			TYPE_ATTRIBUTES (TREE_TYPE (DECL))))		\
    {									\
      fprintf (FILE, "\t.globl __imp_");				\
      assemble_name (FILE, NAME);					\
      fprintf (FILE, "\n__imp_");					\
      assemble_name (FILE, NAME);					\
      fprintf (FILE, ":\n\t.ulong ");					\
      assemble_name (FILE, NAME);					\
      fprintf (FILE, "\n");						\
    }									\
									\
  fprintf (FILE, "\t.section .text\n\t.align 2\n..");			\
  assemble_name (FILE, NAME);						\
  fprintf (FILE, ":\n");						\
  fprintf (FILE, "\t.function\t..");					\
  assemble_name (FILE, NAME);						\
  fprintf (FILE, "\n");							\
}

/* This is how to output an assembler line defining a `double' constant.  */

#undef	ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE, VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE)					\
        || REAL_VALUE_ISNAN (VALUE)					\
	|| REAL_VALUE_MINUS_ZERO (VALUE))				\
      {									\
	long t[2];							\
	REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);			\
	fprintf (FILE, "\t.ualong 0x%lx\n\t.long 0x%lx\n",		\
		t[0] & 0xffffffff, t[1] & 0xffffffff);			\
      }									\
    else								\
      {									\
	char str[30];							\
	REAL_VALUE_TO_DECIMAL (VALUE, "%.20e", str);			\
	fprintf (FILE, "\t.double %s\n", str);				\
      }									\
  }

/* This is how to output an assembler line defining a `float' constant.  */

#undef	ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE, VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE)					\
        || REAL_VALUE_ISNAN (VALUE)					\
	|| REAL_VALUE_MINUS_ZERO (VALUE))				\
      {									\
	long t;								\
	REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);			\
	fprintf (FILE, "\t.ualong 0x%lx\n", t & 0xffffffff);		\
      }									\
    else								\
      {									\
	char str[30];							\
	REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);			\
	fprintf (FILE, "\t.float %s\n", str);				\
      }									\
  }

/* Output before instructions.  */
#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */
#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP "\t.data"

/* Output to the bss section.  */
#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP "\t.section .bss"

/* Define the extra sections we need.  We define a dummy TOC section,
   plus sections to hold the list of static constructors (.ctors) and
   destructors (.dtors).  */

#undef	READONLY_DATA_SECTION
#undef	EXTRA_SECTIONS
#define EXTRA_SECTIONS in_toc, in_ctors, in_dtors

/* Define the routines to implement these extra sections.  */

#undef	EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION						\
  TOC_SECTION_FUNCTION							\

#define TOC_SECTION_FUNCTION						\
void									\
toc_section ()								\
{									\
}

#define CTORS_SECTION_ASM_OP	".section\t.ctors"
#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#define DTORS_SECTION_ASM_OP	".section\t.dtors"
#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

#undef SELECT_SECTION
#undef SELECT_RTX_SECTION

/* Make sure __main gets called */
#define INVOKE__main 1

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#undef	ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t.ualong ");					\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#undef	ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t.ualong ");					\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)


/* Text to write out after a CALL that may be replaced by glue code by
   the loader.  The motorola asm demands that, for dll support, a .znop
   be issued after a bl instruction, and the symbol on the .znop is the
   symbol on the bl instruction */

#undef RS6000_CALL_GLUE
#define RS6000_CALL_GLUE "nop #\tFIXME: only works for non-dll calls."

#define RS6000_CALL_GLUE2 ".znop "

#undef ASM_OUTPUT_SPECIAL_POOL_ENTRY

/* Output something to declare an external symbol to the assembler.  Most
   assemblers don't need this.  */

#undef ASM_OUTPUT_EXTERNAL

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)				\
{									\
  char *_name;								\
  rtx _symref = XEXP (DECL_RTL (DECL), 0);				\
  if ((TREE_CODE (DECL) == VAR_DECL					\
       || TREE_CODE (DECL) == FUNCTION_DECL)				\
      && (NAME)[strlen (NAME) - 1] != ']')				\
    {									\
      _name = (char *) permalloc (strlen (XSTR (_symref, 0)) + 5);	\
      strcpy (_name, XSTR (_symref, 0));				\
      XSTR (_symref, 0) = _name;					\
    }									\
  else									\
    _name = XSTR (_symref, 0);						\
									\
  if (DECL_FUNCTION_CODE (DECL) == 0)					\
    {									\
      fputs ("\t.extern ", FILE);					\
      assemble_name (FILE, _name);					\
      putc ('\n', FILE);						\
      if (TREE_CODE (DECL) == FUNCTION_DECL)				\
	{								\
	  fputs ("\t.extern ..", FILE);					\
	  assemble_name (FILE, _name);					\
	  putc ('\n', FILE);						\
	}								\
    }									\
}

/* Similar, but for libcall.  We only have to worry about the function name,
   not that of the descriptor. */

#undef ASM_OUTPUT_EXTERNAL_LIBCALL

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)	\
{ fprintf (FILE, "\t.extern ..");		\
  assemble_name (FILE, XSTR (FUN, 0));		\
  fprintf (FILE, "\n");				\
}

/* The prefix to add to user-visible assembler symbols. */

#define USER_LABEL_PREFIX ".."

/* Eliminate AIX style constant pool processing */
#undef	LEGITIMATE_CONSTANT_POOL_BASE_P
#define	LEGITIMATE_CONSTANT_POOL_BASE_P(X) 0

#undef	LEGITIMATE_CONSTANT_POOL_ADDRESS_P
#define	LEGITIMATE_CONSTANT_POOL_ADDRESS_P(X) 0

#undef ASM_OUTPUT_SPECIAL_POOL_ENTRY

#undef  ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(x)

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.  */
#define TRAMPOLINE_TEMPLATE(FILE) rs6000_trampoline_template (FILE)
