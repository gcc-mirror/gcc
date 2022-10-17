/* Test AAPCS64 layout */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_dfp_22.c"

struct y
{
  _Decimal32 p;
  _Decimal32 q;
} v = { 345.0df, 678.0df };

#include "abitest.h"
#else
  ARG(_Decimal32, 123.0df, S0)
  ARG(struct y, v, S1)
  LAST_ARG(_Decimal32, 901.0df, S3)
#endif
