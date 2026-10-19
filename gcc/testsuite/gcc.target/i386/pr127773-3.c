/* PR target/127773 */
/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include "avx2-check.h"

typedef long long v4di __attribute__ ((vector_size (32)));
typedef double v4df __attribute__ ((vector_size (32)));

static __attribute__ ((noinline)) int
f (int x)
{
  return x * 3 + 1;
}

static __attribute__ ((noinline)) double
fd (double x)
{
  return x * 3.0;
}

/* Check that vzeroupper does not clobber vectors live across calls.  */

__attribute__ ((noinline)) void
g1 (v4di *p, int x)
{
  v4di t = *p + *p;
  int r = f (x);
  *p = t + r;
}

__attribute__ ((noinline)) double
g2 (v4df *p, double d, int c)
{
  v4df t = *p * 2.0;
  double s = 0;
  for (int i = 0; i < c; i++)
    {
      s += fd (d + i);
      if (i & 1)
	t += s;
    }
  *p = t;
  return s;
}

static void
avx2_test (void)
{
  v4di a = { 1, 2, 3, 4 };
  g1 (&a, 5);
  if (a[0] != 18 || a[3] != 24)
    abort ();

  v4df b = { 1, 2, 3, 4 };
  double s = g2 (&b, 1.0, 4);
  if (s != 3.0 * (1 + 2 + 3 + 4) || b[3] != 8.0 + 9.0 + 30.0)
    abort ();
}
