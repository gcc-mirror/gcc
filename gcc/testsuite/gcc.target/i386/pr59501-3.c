/* PR target/59501 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx -mno-accumulate-outgoing-args" } */

#define CHECK_H "avx-check.h"
#define TEST avx_test

#include CHECK_H

typedef double V __attribute__ ((vector_size (32)));

__attribute__((noinline, noclone)) V
foo (double *x, int a, int b, int c, int d, int e, int f, unsigned *y)
{
  V r = { x[y[0]], x[y[1]], x[y[2]], x[y[3]] };
  return r;
}

static void
TEST (void)
{
  double a[16];
  unsigned b[4] = { 5, 0, 15, 7 };
  int i;
  for (i = 0; i < 16; i++)
    a[i] = 0.5 + i;
  V v = foo (a, 0, 0, 0, 0, 0, 0, b);
  if (v[0] != 5.5 || v[1] != 0.5 || v[2] != 15.5 || v[3] != 7.5)
    __builtin_abort ();
}
