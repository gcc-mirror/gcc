/* { dg-do run } */
/* { dg-options "-O3 -mavx512f -mprefer-vector-width=512" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>
#include "avx512f-check.h"
#include "pr128221-1.c"

#define W 1800.0
#define E -1e34

static void
check (void (*fn) (double, double), int op)
{
  for (int i = 0; i < N; i++)
    {
      m[i] = i & 1;
      /* Overflows only in the lanes where m[i] is zero.  */
      b[i] = m[i] ? 1.0 : 1e307;
      a[i] = 2.0;
      c[i] = 3.0;
    }

  feclearexcept (FE_ALL_EXCEPT);
  fn (W, E);
  if (fetestexcept (FE_OVERFLOW))
    abort ();

  for (int i = 0; i < N; i++)
    {
      double r;
      if (!m[i])
	r = E;
      else if (op == 0)
	r = 2.0 + W;
      else if (op == 1)
	r = W - 3.0;
      else if (op == 2)
	r = 3.0 - W;
      else
	r = -W - 3.0;
      if (a[i] != r)
	abort ();
    }
}

static void
avx512f_test (void)
{
  check (fma_test, 0);
  check (fms_test, 1);
  check (fnma_test, 2);
  check (fnms_test, 3);
}
