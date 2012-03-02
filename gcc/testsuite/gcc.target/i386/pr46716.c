/* { dg-do run } */
/* { dg-options "-O2 -msse -mno-sse2" } */
/* { dg-require-effective-target sse } */

#include "sse-check.h"

typedef double V __attribute__ ((__vector_size__ (16), __may_alias__));
typedef union
{
  V x;
  double a[2];
} u;

#define EMM_FLT8(a) ((double *)&(a))

void __attribute__ ((noinline))
test (V s1, V s2)
{
  if (EMM_FLT8(s1)[0] != EMM_FLT8(s2)[0]
      || EMM_FLT8(s1)[1] != EMM_FLT8(s2)[1])
    abort ();
}

static void
sse_test (void)
{
  u s1;

  s1.a[0] = 1.0;
  s1.a[1] = 2.0;

  test (s1.x, s1.x);
}
