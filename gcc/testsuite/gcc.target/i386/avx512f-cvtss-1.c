/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx512f-check.h"

__attribute__((noinline, noclone)) double
foo (__m512 x)
{
  return _mm512_cvtss_f32 (x);
}

static void
avx512f_test (void)
{
  if (_mm512_cvtss_f32 (_mm512_set_ps (13.0f, 24.5f, 23.0f, 22.5f,
				       2.0f, 3.0f, 4.0f, 5.0f,
				       6.0f, 7.0f, 8.0f, 9.0f,
				       10.0f, 11.0f, 12.0f, 13.5f)) != 13.5f)
    __builtin_abort ();

  if (foo (_mm512_set_ps (13.25f, 24.25f, 23.75f, 22.0f,
			  2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f,
			  10.0f, 11.0f, 12.0f, 12.25f)) != 12.25f)
    __builtin_abort ();
}
