#include "mips/xm-mips.h"

#define USG
#define HAVE_VPRINTF

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

#if 0
#ifdef __GNUC__
/* The normal irix compiler requires alloca.h or alloca doesn't work.
   However, the IRIX compiler doesn't allow alloca to be stored in
   something like ptr->field = alloca(), so we just use the normal
   C alloca.  */
#include <alloca.h>
#endif
#endif
