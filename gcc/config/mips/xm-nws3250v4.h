#define USG

#include "xm-mips.h"

/* If compiling with mips compiler, we are probably using alloca.c,
   so help it work right.  */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif
