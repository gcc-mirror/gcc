/* Test AAPCS layout (VFP variant) */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define VFP
#define TESTFILE "test_dfp_17.c"

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
  _Decimal64 x[4];
};

_Decimal32 f1 = 25.0df;
struct z a = { 5.0dd, 6.0dd, 7.0dd, 8.0dd };
struct z b = { 9.0dd, 10.0dd, 11.0dd, 12.0dd };

#include "abitest.h"
#else
  ARG(_Decimal64, 11.0dd, D0)
  DOTS
  ANON(struct z, a, D1)
  ANON(struct z, b, STACK)
  ANON(int , 5, W0)
#ifndef __AAPCS64_BIG_ENDIAN__
  ANON(_Decimal32, f1, STACK+32) /* Note: no promotion to _Decimal64.  */
#else
  ANON(_Decimal32, f1, STACK+36) /* Note: no promotion to _Decimal64.  */
#endif
  LAST_ANON(_Decimal64, 0.5dd, STACK+40)
#endif
