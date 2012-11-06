/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_10.c"

struct z
{
  double x[4];
};

struct z a = { 5.0, 6.0, 7.0, 8.0 };
struct z b = { 9.0, 10.0, 11.0, 12.0 };

#include "abitest.h"
#else

  ARG(int, 7, W0)
  DOTS
  ANON(struct z, a, D0)
  ANON(struct z, b, D4)
  ANON(double, 0.5, STACK)
  LAST_ANON(double, 1.5, STACK+8)
#endif
