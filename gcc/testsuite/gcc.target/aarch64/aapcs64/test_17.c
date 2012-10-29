/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_17.c"

__complex__ x = 1.0+2.0i;

struct y
{
  int p;
  int q;
  int r;
  int s;
} v = { 1, 2, 3, 4 };

struct z
{
  double x[4];
};

float f1 = 25.0;
struct z a = { 5.0, 6.0, 7.0, 8.0 };
struct z b = { 9.0, 10.0, 11.0, 12.0 };

#include "abitest.h"
#else
  ARG(double, 11.0, D0)
  DOTS
  ANON(struct z, a, D1)
  ANON(struct z, b, STACK)
  ANON(int , 5, W0)
  ANON(double, f1, STACK+32)
  LAST_ANON(double, 0.5, STACK+40)
#endif
