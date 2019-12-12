/* Test AAPCS layout (alignment).  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "overalign_rec3.c"

typedef struct
{
  int  __attribute__((aligned(16))) a;
  int b;
} overaligned;

overaligned v = {1, 3};
overaligned w = {33, 99};

#include "abitest.h"
#else
  ARG (int, 7, R0)
  /* Objects with alignment > 8 are passed with alignment 8.  */
  ARG (overaligned, v, R2)
  ARG (int, 9, STACK+8)
  ARG (int, 10, STACK+12)
  ARG (int, 11, STACK+16)
  LAST_ARG (overaligned, w, STACK+24)
#endif
