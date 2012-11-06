/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_int128.c"

typedef int TItype __attribute__ ((mode (TI)));

TItype x = 0xcafecafecafecfeacfeacfea;
TItype y = 0xcfeacfeacfeacafecafecafe;

#include "abitest.h"
#else
  ARG (TItype, x, X0)
  LAST_ARG (TItype, y, X2)
#endif
