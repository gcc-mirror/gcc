#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dvax -D__NetBSD__ -Asystem=unix -Asystem=NetBSD -Acpu=vax -Amachine=vax"

/* Make gcc agree with <machine/ansi.h> */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* Until they use ELF or something that handles dwarf2 unwinds
   and initialization stuff better.  */
#undef DWARF2_UNWIND_INFO

