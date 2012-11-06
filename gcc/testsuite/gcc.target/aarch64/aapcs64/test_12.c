/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_12.c"


struct y
{
  long p;
  long q;
  long r;
  long s;
} v = { 1, 2, 3, 4 };

struct y1
{
  int p;
  int q;
  int r;
  int s;
} v1 = { 1, 2, 3, 4 };


struct z
{
  double x[4];
};

struct z a = { 5.0, 6.0, 7.0, 8.0 };
struct z b = { 9.0, 10.0, 11.0, 12.0 };

#define MYFUNCTYPE struct y

#include "abitest.h"
#else
  ARG(int, 7, W0)
  ARG(struct y1, v1, X1)
  ARG(struct z, a, D0)
  ARG(struct z, b, D4)
  LAST_ARG(double, 0.5, STACK)
#endif
