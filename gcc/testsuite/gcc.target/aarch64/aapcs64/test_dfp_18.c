/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK

#define TESTFILE "test_dfp_18.c"


struct y
{
  long long p;
  long long q;
  long long r;
  long long s;
} v = { 1, 2, 3, 4 };

struct z
{
  _Decimal64 x[4];
};

struct z a = { 5.0dd, 6.0dd, 7.0dd, 8.0dd };
struct z b = { 9.0dd, 10.0dd, 11.0dd, 12.0dd };

#include "abitest.h"
#else
  ARG(int, 7, W0)
  PTR(struct y, v, X1)
  ARG(struct z, a, D0)
  ARG(_Decimal64, 1.0dd, D4)
  ARG(struct z, b, STACK)
  LAST_ARG(_Decimal64, 0.5dd, STACK+32)
#endif
