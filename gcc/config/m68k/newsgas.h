/* In Sony versions before 3.0, use the GNU Assembler, because the
   system's assembler has no way to assemble the difference of two
   labels for the displacement in a switch-dispatch instruction.  */  

#define USE_GAS

/* This is the assembler directive to equate two values.  */

#undef SET_ASM_OP
#define SET_ASM_OP    "\t.set\t"

/* This is how we tell the assembler that a symbol is weak.  */

#undef ASM_WEAKEN_LABEL
#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
       fputc ('\n', FILE); } while (0)

#include "m68k/news.h"
