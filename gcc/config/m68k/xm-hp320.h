/* USG is needed to prevent trying to use getrusage and getwd.  */
#define USG

#include "m68k/xm-m68k.h"

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)
#define rindex strrchr
#define index strchr

/* If compiling with HPUX compiler, we are probably using alloca.c,
   so help it work right.  */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif

/* Don't try to use sys_siglist.  */
#define NO_SYS_SIGLIST
