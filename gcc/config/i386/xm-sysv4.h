/* Configuration for GCC for Intel i386 running System V Release 4.  */

#include "i386/xm-i386.h"
#include "xm-svr4.h"

/* If not compiled with GNU C, use the portable alloca.  */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif
