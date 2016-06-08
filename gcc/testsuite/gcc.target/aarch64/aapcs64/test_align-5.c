/* Test AAPCS layout (alignment).  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-5.c"

typedef __attribute__ ((__aligned__ (16))) long alignedint;

alignedint a = 11;
alignedint b = 13;
alignedint c = 17;
alignedint d = 19;
alignedint e = 23;
alignedint f = 29;
alignedint g = 31;
alignedint h = 37;
alignedint i = 41;
alignedint j = 43;

#include "abitest.h"
#else
  ARG (alignedint, a, X0)
  /* Attribute suggests R2, but we should use only natural alignment:  */
  ARG (alignedint, b, X1)
  ARG (alignedint, c, X2)
  ARG (alignedint, d, X3)
  ARG (alignedint, e, X4)
  ARG (alignedint, f, X5)
  ARG (alignedint, g, X6)
  ARG (alignedint, h, X7)
  ARG (alignedint, i, STACK)
  /* Attribute would suggest STACK + 16 but should be ignored:  */
  LAST_ARG (alignedint, j, STACK + 8)
#endif
