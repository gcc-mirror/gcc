#include "mips/xm-mips.h"

#define USG
#define HAVE_VPRINTF

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

