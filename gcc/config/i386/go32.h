/* Configuration for an i386 running MS-DOS with djgpp/go32.  */

#define YES_UNDERSCORES

#include "i386/gas.h"

#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
#define CPP_PREDEFINES "-Dunix -Di386 -DGO32 -DMSDOS"
