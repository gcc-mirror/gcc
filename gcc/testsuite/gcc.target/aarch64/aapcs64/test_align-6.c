/* Test AAPCS layout (alignment).  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-6.c"

/* The underlying struct here has alignment 8.  */
typedef struct __attribute__ ((__aligned__ (16)))
  {
    long x;
    long y;
  } overaligned;

/* A couple of instances, at 16-byte-aligned memory locations.  */
overaligned a = { 2, 3 };
overaligned b = { 5, 8 };
overaligned c = { 13, 21 };

#include "abitest.h"
#else
  ARG (int, 7, W0)
  /* Natural alignment should be 8.  */
  ARG (overaligned, a, X1)
  ARG (int, 9, W3)
  ARG (int, 11, W4)
  ARG (overaligned, b, X5)
  ARG (int, 15, W7)
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG (int, 10, STACK)
#else
  ARG (int, 10, STACK + 4)
#endif
  /* Natural alignment should be 8.  */
  LAST_ARG (overaligned, c, STACK + 8)
#endif
