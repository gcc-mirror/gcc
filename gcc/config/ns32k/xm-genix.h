/* Config file for ns32k running system V.  */

#include "ns32k/xm-ns32k.h"

#define USG

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)
