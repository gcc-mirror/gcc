/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_dfp_12.c"


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
  _Decimal64 x[4];
};

struct z a = { 5.0dd, 6.0dd, 7.0dd, 8.0dd };
struct z b = { 9.0dd, 10.0dd, 11.0dd, 12.0dd };

#define MYFUNCTYPE struct y

#include "abitest.h"
#else
  ARG(int, 7, W0)
  ARG(struct y1, v1, X1)
  ARG(struct z, a, D0)
  ARG(struct z, b, D4)
  LAST_ARG(_Decimal64, 0.5dd, STACK)
#endif
