/* Enable debugging.  */
#define DBX_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO
#define MIPS_DEBUGGING_INFO

#define DWARF_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG

/* We need to use .esize and .etype instead of .size and .type to
   avoid conflicting with ELF directives.  These are only recognized
   by gas, anyhow, not the native assembler.  */
#undef PUT_SDB_SIZE
#define PUT_SDB_SIZE(a)                               \
do {                                                  \
  extern FILE *asm_out_text_file;                     \
  fprintf (asm_out_text_file, "\t.esize\t");          \
  fprintf (asm_out_text_file, HOST_WIDE_INT_PRINT_DEC, (HOST_WIDE_INT) (a)); \
  fprintf (asm_out_text_file, ";");                   \
} while (0)

#undef PUT_SDB_TYPE
#define PUT_SDB_TYPE(a)                                       \
do {                                                  \
  extern FILE *asm_out_text_file;                     \
  fprintf (asm_out_text_file, "\t.etype\t0x%x;", (a));        \
} while (0)


/* This is how to equate one symbol to another symbol.  The syntax used is
   `SYM1=SYM2'.  Note that this is different from the way equates are done
   with most svr4 assemblers, where the syntax is `.set SYM1,SYM2'.  */

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {	fprintf ((FILE), "\t");						\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, " = ");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)
