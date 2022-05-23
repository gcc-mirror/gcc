/* Test AAPCS64 layout.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_dfp_19.c"

struct y
{
  int p1;
  int p2;
  _Decimal32 q;
  int r1;
  int r2;
  char x;
} v = { -1, 1, 2.0df, 3, 18, 19, 20};

struct z
{
  _Decimal64 x[4];
};

struct z a = { 5.0dd, 6.0dd, 7.0dd, 8.0dd };
struct z b = { 9.0dd, 10.0dd, 11.0dd, 12.0dd };

#include "abitest.h"
#else
  ARG(int, 7, W0)
  DOTS
  ANON(_Decimal64, 4.0dd, D0)
  ANON(struct z, a, D1)
  ANON(struct z, b, STACK)
  PTR_ANON(struct y, v, X1)
  LAST_ANON(int, 10, W2)
#endif
