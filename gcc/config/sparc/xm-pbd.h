/* Host environment for the tti "Unicom" PBB 68020 boards */

#include "sparc/xm-sparc.h"

#define USG
#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

#ifndef __GNUC__
#define USE_C_ALLOCA
#endif

