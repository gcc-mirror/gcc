/* Test AAPCS layout (alignment).  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "overalign_rec1.c"

typedef struct __attribute__((aligned(8)))
{
  int a;
  int b;
} overaligned;

overaligned v = {1, 3};
overaligned w = {33, 99};

#include "abitest.h"
#else
  ARG (int, 7, R0)
  /* Overalignment is ignored for the purposes of parameter passing.  */
  ARG (overaligned, v, R1)
  ARG (int, 11, R3)
  ARG (int, 9, STACK)
  LAST_ARG (overaligned, w, STACK+4)
#endif
