/* Configuration for an i386 running MS-DOS with djgpp/go32.  */

#define DBX_DEBUGGING_INFO /* support for stabs debugging info */
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG /* leave sdb as default */
#define NO_STAB_H /* DJGPP has no stab.h */
#if 0 /* enable this, if '-g' should select stabs debugging */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#endif

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

#define HANDLE_SYSV_PRAGMA

#define YES_UNDERSCORES

#include "i386/gas.h"

#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
#define CPP_PREDEFINES "-Dunix -Di386 -DGO32 -DMSDOS \
  -Asystem(unix) -Asystem(msdos) -Acpu(i386) -Amachine(i386)"

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
      fprintf (asm_out_file, "\t.section .ctor\n");		\
      in_section = in_ctor;					\
    }								\
}

#define DTOR_SECTION_FUNCTION					\
void								\
dtor_section ()							\
{								\
  if (in_section != in_dtor)					\
    {								\
      fprintf (asm_out_file, "\t.section .dtor\n");		\
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
#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME)\
  do {						\
    fprintf (FILE, "\t.section %s\n", NAME);	\
  } while (0)
  do {						\
    dtor_section ();                   		\
    fprintf (FILE, "%s\t", ASM_LONG);		\
    assemble_name (FILE, NAME);              	\
    fprintf (FILE, "\n");			\
  } while (0)

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */
/* Use the main_input_filename instead of dump_base_name */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
  do {									\
	output_file_directive (FILE, main_input_filename);		\
  } while (0)

/* Be function-relative for block and source line stab directives. */

#define DBX_BLOCKS_FUNCTION_RELATIVE 1

/* but, to make this work, functions must appear prior to line info */

#define DBX_FUNCTION_FIRST
/* Allow (eg) __attribute__((section "locked")) to work */
/* Generate a blank trailing N_SO to mark the end of the .o file, since
   we can't depend upon the linker to mark .o file boundaries with
   embedded stabs.  */

#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)			\
  fprintf (FILE,							\
	   "\t.text\n\t.stabs \"\",%d,0,0,Letext\nLetext:\n", N_SO)

#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  if ( write_symbols == DBX_DEBUG )                     \
  { static int sym_lineno = 1;				\
    fprintf (file, ".stabn 68,0,%d,.LM%d-",		\
	     line, sym_lineno);				\
    assemble_name (file,				\
		   XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0)); \
    fprintf (file, "\n.LM%d:\n", sym_lineno);		\
    sym_lineno += 1; } \
  else { \
    fprintf (file, "\t.ln\t%d\n", \
  	     ((sdb_begin_function_line > -1) \
	     ? line - sdb_begin_function_line : 1)); }
#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME)\
  do {						\
    fprintf (FILE, "\t.section %s\n", NAME);	\
  } while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG) != 0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))
