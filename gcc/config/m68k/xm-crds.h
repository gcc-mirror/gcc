#define USG

#ifndef unos
#define unos
#endif

#include "m68k/xm-m68k.h"

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

/* UNOS has vprintf() */ 
#define HAVE_VPRINTF

/* Avoid conflict with C library by changing name of this symbol.  */
#define gettime gcc_gettime

#ifndef __GNUC__
#define USE_C_ALLOCA
#endif

