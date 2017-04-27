/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx512f-check.h"

__attribute__((noinline, noclone)) double
foo (__m512d x)
{
  return _mm512_cvtsd_f64 (x);
}

static void
avx512f_test (void)
{
  if (_mm512_cvtsd_f64 (_mm512_set_pd (5.5, 24.5, 23.0, 22.5,
				       2.0, 3.0, 4.0, 13.5)) != 13.5)
    __builtin_abort ();

  if (foo (_mm512_set_pd (5.25, 24.25, 23.75, 22.0,
			  2.0, 3.0, 4.0, 12.25)) != 12.25)
    __builtin_abort ();
}
