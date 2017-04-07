/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

__attribute__((noinline, noclone)) double
foo (__m256d x)
{
  return _mm256_cvtsd_f64 (x);
}

static void
avx_test (void)
{
  if (_mm256_cvtsd_f64 (_mm256_set_pd (13.5, 24.5, 23.0, 22.5)) != 22.5)
    __builtin_abort ();

  if (foo (_mm256_set_pd (24.25, 23.75, 22.0, 12.25)) != 12.25)
    __builtin_abort ();
}
