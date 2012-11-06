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
#define TESTFILE "test_16.c"

#include "abitest.h"
#else
  ARG(float, 1.0, S0)
  ARG(float, 2.0, S1)
  ARG(float, 3.0, S2)
  ARG(float, 4.0, S3)
  ARG(float, 5.0, S4)
  ARG(float, 6.0, S5)
  ARG(float, 7.0, S6)
  ARG(float, 8.0, S7)
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG(float, 9.0, STACK)
  LAST_ARG(float, 10.0, STACK+8)
#else
  ARG(float, 9.0, STACK+4)
  LAST_ARG(float, 10.0, STACK+12)
#endif
#endif
