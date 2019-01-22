/* Test AAPCS layout (alignment).  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "bitfield1.c"

struct bf
{
  unsigned long long a: 61;
  unsigned b: 3;
} v = {1, 1};

#include "abitest.h"
#else
  ARG (int, 7, R0)
  ARG (int, 9, R1)
  ARG (int, 11, R2)
  /* Alignment of the bitfield type should affect alignment of the overall
     type, so R3 not used.  */
  LAST_ARG (struct bf, v, STACK)
#endif
