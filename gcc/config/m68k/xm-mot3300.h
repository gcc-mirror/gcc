#define USG

#include "m68k/xm-m68k.h"

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

#define rindex strrchr
#define index strchr

#define NO_SYS_SIGLIST

#ifndef __GNUC__
#define USE_C_ALLOCA
#endif
