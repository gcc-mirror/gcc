/* Test AAPCS64 layout */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_22.c"

struct y
{
  float p;
  float q;
} v = { 345.0f, 678.0f };

#include "abitest.h"
#else
  ARG(float, 123.0f, S0)
  ARG(struct y, v, S1)
  LAST_ARG(float, 901.0f, S3)
#endif
