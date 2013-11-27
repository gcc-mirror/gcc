/* { dg-do run } */
/* { dg-require-effective-target xop } */
/* { dg-options "-O2 -mxop" } */

#include "xop-check.h"

#include <x86intrin.h>

void
check_mm_vmfrcz_sd (__m128d __A, __m128d __B)
{
  union128d a, b, c;
  double d[2];

  a.x = __A;
  b.x = __B;
  c.x = _mm_frcz_sd (__A, __B);
  d[0] = b.a[0] - (int)b.a[0] ;
  d[1] = a.a[1];
  if (check_union128d (c, d))
    abort ();
}

void
check_mm_vmfrcz_ss (__m128 __A, __m128 __B)
{
  union128 a, b, c;
  float f[4];

  a.x = __A;
  b.x = __B;
  c.x = _mm_frcz_ss (__A, __B);
  f[0] = b.a[0] - (int)b.a[0] ;
  f[1] = a.a[1];
  f[2] = a.a[2];
  f[3] = a.a[3];
  if (check_union128 (c, f))
    abort ();
}

static void
xop_test (void)
{
  union128 a, b;
  union128d c,d;
  int i;

  for (i = 0; i < 4; i++)
    {
       a.a[i] = i + 3.5;
       b.a[i] = i + 7.9;
    }
  for (i = 0; i < 2; i++)
    {
       c.a[i] = i + 3.5;
       d.a[i] = i + 7.987654321;
    }
  check_mm_vmfrcz_ss (a.x, b.x);
  check_mm_vmfrcz_sd (c.x, d.x);
}
