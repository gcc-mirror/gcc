#define MIPS_OVERRIDE_ALLOCA
#ifndef __GNUC__
#include <alloca.h>
#else
extern void *alloca ();
#endif

#include "mips/xm-mips.h"

#define USG

#undef HOST_BITS_PER_LONG
#define HOST_BITS_PER_LONG	_MIPS_SZLONG

#ifndef inhibit_libc
#include "string.h"
#endif
