/* Configuration for GCC for Intel i386 running System V Release 4.  */

#include "xm-i386.h"
#include "xm-svr4.h"

/* If compiled with GNU C, use the built-in alloca.  */
#undef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#define USE_C_ALLOCA
#endif
