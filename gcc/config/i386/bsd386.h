/* Configuration for an i386 running BSDI's BSD/OS (formerly known as BSD/386)
   as the target machine.  */

#include "i386/386bsd.h"

/* We exist mostly to add -Dbsdi and such to the predefines. */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -Dbsdi -D__i386__ -D__bsdi__ -D____386BSD____ -D__386BSD__ -DBSD_NET2 -Asystem(unix) -Asystem(bsd) -Acpu(i386) -Amachine(i386)"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* This is suitable for BSD/OS 3.0; we don't know about earlier releases.  */
#undef ASM_COMMENT_START
#define ASM_COMMENT_START " #"

/* Until they use ELF or something that handles dwarf2 unwinds
   and initialization stuff better.  */
#define DWARF2_UNWIND_INFO 0

/* BSD/OS still uses old binutils that don't insert nops by default
   when the .align directive demands to insert extra space in the text
   segment.  */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG)!=0) fprintf ((FILE), "\t.align %d,0x90\n", (LOG))
