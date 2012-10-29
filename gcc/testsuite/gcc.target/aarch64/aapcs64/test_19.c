/* Test AAPCS64 layout.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_19.c"

struct y
{
  int p1;
  int p2;
  float q;
  int r1;
  int r2;
  char x;
} v = { -1, 1, 2.0f, 3, 18, 19, 20};

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
  ANON(double, 4.0, D0)
  ANON(struct z, a, D1)
  ANON(struct z, b, STACK)
  PTR_ANON(struct y, v, X1)
  LAST_ANON(int, 10, W2)
#endif
