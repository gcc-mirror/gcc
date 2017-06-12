/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

__attribute__((noinline, noclone)) double
foo (__m256 x)
{
  return _mm256_cvtss_f32 (x);
}

static void
avx_test (void)
{
  if (_mm256_cvtss_f32 (_mm256_set_ps (5.5f, 24.5f, 23.0f, 22.5f,
				       2.0f, 3.0f, 4.0f, 13.5f)) != 13.5f)
    __builtin_abort ();

  if (foo (_mm256_set_ps (5.25f, 24.25f, 23.75f, 22.0f,
			  2.0f, 3.0f, 4.0f, 12.25f)) != 12.25f)
    __builtin_abort ();
}
