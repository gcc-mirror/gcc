/* Host environment for 68000's running System V.  */

#include "m68k/xm-m68k.h"

#define USG
#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

#define rindex strrchr
#define index strchr

#ifndef __GNUC__
#define USE_C_ALLOCA
#endif
