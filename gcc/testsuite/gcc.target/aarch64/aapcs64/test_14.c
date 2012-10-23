/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_14.c"


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

struct z a = { 5.0, 6.0, 7.0, 8.0 };
struct z b = { 9.0, 10.0, 11.0, 12.0 };

#include "abitest.h"
#else
  ARG(int, 7, W0)
  ARG(int, 9, W1)
  ARG(struct z, a, D0)
  ARG(double, 1.0, D4)
  ARG(struct z, b, STACK)
  ARG(int, 4, W2)
  LAST_ARG(double, 0.5, STACK+32)
#endif
