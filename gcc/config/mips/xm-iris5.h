#include "mips/xm-mips.h"

#define USG

/* On SGI IRIX 5.3, inttypes.h clashes with sys/types.h, but the clash
   (when compiled with GCC) is a warning, so configure.in thinks it's OK
   to use it.  Work around this problem.  */

#ifdef HAVE_INTTYPES_H
#undef HAVE_INTTYPES_H
#endif
