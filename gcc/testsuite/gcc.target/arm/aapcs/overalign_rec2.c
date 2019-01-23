/* Test AAPCS layout (alignment).  */

/* { dg-do run { target arm_eabi } } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-O" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "overalign_rec2.c"

typedef struct
{
  int  __attribute__((aligned(8))) a;
  int b;
} overaligned;

overaligned v = {1, 3};
overaligned w = {33, 99};

#include "abitest.h"
#else
  ARG (int, 7, R0)
  ARG (overaligned, v, R2)
  ARG (int, 9, STACK)
  LAST_ARG (overaligned, w, STACK+8)
#endif
