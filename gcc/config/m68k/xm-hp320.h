/* USG is needed to prevent trying to use getrusage and getwd.  */
#define USG

#include "m68k/xm-m68k.h"

/* If compiling with HPUX compiler, we are probably using alloca.c,
   so help it work right.  */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif

/* Don't try to use sys_siglist.  */
#define NO_SYS_SIGLIST
