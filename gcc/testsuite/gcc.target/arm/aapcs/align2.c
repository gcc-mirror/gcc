/* Test AAPCS layout (alignment).  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "align2.c"

/* The underlying struct here has alignment 4.  */
typedef struct __attribute__((aligned (8)))
  {
    int x;
    int y;
  } overaligned;

/* A couple of instances, at 8-byte-aligned memory locations.  */
overaligned a = { 2, 3 };
overaligned b = { 5, 8 };

#include "abitest.h"
#else
  ARG (int, 7, R0)
  /* Alignment should be 4.  */
  ARG (overaligned, a, R1)
  ARG (int, 9, R3)
  ARG (int, 10, STACK)
  /* Alignment should be 4.  */
  LAST_ARG (overaligned, b, STACK + 4)
#endif
