/* Configuration for an i386 running MS-DOS with djgpp/go32.  */

#include "dbxcoff.h"

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
/* Use the main_input_filename instead of dump_base_name */

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

/* djgpp has atexit ().  */
#undef HAVE_ATEXIT
#define HAVE_ATEXIT

/* djgpp automatically calls its own version of __main, so don't define one
   in libgcc, nor call one in main().  */
#define HAS_INIT_SECTION
