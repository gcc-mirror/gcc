/* Definitions of target machine for GNU compiler.  Irix version 5 with gas. */

#include "mips/iris5.h"

/* Enable debugging.  */
#define DBX_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO
#define MIPS_DEBUGGING_INFO

/* GNU as does handle DWARF2 directives.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

/* Irix 5 does not have some strange restrictions that Irix 3 had.  */
#undef SET_FILE_NUMBER
#define SET_FILE_NUMBER() ++num_source_filenames
#undef LABEL_AFTER_LOC
#define LABEL_AFTER_LOC(STREAM)

/* We need to use .esize and .etype instead of .size and .type to
   avoid conflicting with ELF directives.  These are only recognized
   by gas, anyhow, not the native assembler.  */
#undef PUT_SDB_SIZE
#define PUT_SDB_SIZE(a)                                       \
do {                                                  \
  extern FILE *asm_out_text_file;                     \
  fprintf (asm_out_text_file, "\t.esize\t%d;", (a));  \
} while (0)

#undef PUT_SDB_TYPE
#define PUT_SDB_TYPE(a)                                       \
do {                                                  \
  extern FILE *asm_out_text_file;                     \
  fprintf (asm_out_text_file, "\t.etype\t0x%x;", (a));        \
} while (0)
