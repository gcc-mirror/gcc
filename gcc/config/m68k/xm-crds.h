#define USG

#ifndef unos
#define unos
#endif

#include "m68k/xm-m68k.h"

/* Avoid conflict with C library by changing name of this symbol.  */
#define gettime gcc_gettime

#ifndef __GNUC__
#define USE_C_ALLOCA
#endif

/* Override part of the obstack macros.  */

#define __PTR_TO_INT(P) ((int)(P))
#define __INT_TO_PTR(P) ((char *)(P))
