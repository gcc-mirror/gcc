#include "mips/xm-mips.h"

#define USG

#undef HOST_BITS_PER_LONG
#define HOST_BITS_PER_LONG	_MIPS_SZLONG

#ifndef inhibit_libc
#include "string.h"
#endif
