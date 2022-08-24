/* Test AAPCS layout */
/* C.5 If the argument is a Half- or Single- precision Floating-point type,
   then the size of the argument is set to 8 bytes.  The effect is as if
   the argument had been copied to the least significant bits of a 64-bit
   register and the remaining bits filled with unspecified values.  */
/* TODO: add the check of half-precision floating-point when it is supported
   by the A64 GCC.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_dfp_16.c"

#include "abitest.h"
#else
  ARG(_Decimal32, 1.0df, S0)
  ARG(_Decimal32, 2.0df, S1)
  ARG(_Decimal32, 3.0df, S2)
  ARG(_Decimal32, 4.0df, S3)
  ARG(_Decimal32, 5.0df, S4)
  ARG(_Decimal32, 6.0df, S5)
  ARG(_Decimal32, 7.0df, S6)
  ARG(_Decimal32, 8.0df, S7)
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG(_Decimal32, 9.0df, STACK)
  LAST_ARG(_Decimal32, 10.0df, STACK+8)
#else
  ARG(_Decimal32, 9.0df, STACK+4)
  LAST_ARG(_Decimal32, 10.0df, STACK+12)
#endif
#endif
