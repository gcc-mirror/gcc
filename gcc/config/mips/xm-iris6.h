#define MIPS_OVERRIDE_ALLOCA
#ifndef __GNUC__
#include <alloca.h>
#else
extern void *alloca ();
#endif

#include "mips/xm-iris5.h"

#undef HOST_BITS_PER_LONG
#define HOST_BITS_PER_LONG	64

/* Declare some functions needed for this machine.  We don't want to
   include these in the sources since other machines might define them
   differently.  */

extern void *malloc (), *realloc (), *calloc ();

#ifndef inhibit_libc
#include "string.h"
#endif
