/* Test AAPCS layout (alignment).  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "align1.c"

typedef __attribute__((aligned (8))) int alignedint;

alignedint a = 11;
alignedint b = 13;
alignedint c = 17;
alignedint d = 19;
alignedint e = 23;
alignedint f = 29;

#include "abitest.h"
#else
  ARG (alignedint, a, R0)
  /* Attribute suggests R2, but we should use only natural alignment:  */
  ARG (alignedint, b, R1)
  ARG (alignedint, c, R2)
  ARG (alignedint, d, R3)
  ARG (alignedint, e, STACK)
  /* Attribute would suggest STACK + 8 but should be ignored:  */
  LAST_ARG (alignedint, f, STACK + 4)
#endif
