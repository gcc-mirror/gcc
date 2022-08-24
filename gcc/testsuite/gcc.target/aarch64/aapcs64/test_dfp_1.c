/* Test AAPCS64 layout */

/* C.7  If the argument is an Integral Type, the size of the argument is
	less than or equal to 8 bytes and the NGRN is less than 8, the
	argument is copied to the least significant bits in x[NGRN].  The
	NGRN is incremented by one.  The argument has now been allocated.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_dfp_1.c"
/* TODO: review if we need this */
#define RUNTIME_ENDIANNESS_CHECK
#include "abitest.h"
#else
  ARG(int, 4, W0)
  ARG(_Decimal64, 4.0dd, D0)
  ARG(int, 3, W1)
  /* TODO: review the way of memcpy char, short, etc.  */
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG(char, 0xEF, X2)
  ARG(short, 0xBEEF, X3)
  ARG(int, 0xDEADBEEF, X4)
#else
  /* TODO: need the model/qemu to be big-endian as well  */
  ARG(char, 0xEF, X2+7)
  ARG(short, 0xBEEF, X3+6)
  ARG(int, 0xDEADBEEF, X4+4)
#endif
  LAST_ARG(long long, 0xDEADBEEFCAFEBABELL, X5)
#endif
