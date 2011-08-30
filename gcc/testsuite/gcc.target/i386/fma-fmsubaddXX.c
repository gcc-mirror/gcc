/* { dg-do run } */
/* { dg-require-effective-target fma } */
/* { dg-options "-O2 -mfma" } */

#include "fma-check.h"

#include <x86intrin.h>
#include "m256-check.h"

void
check_mm_fmsubadd_ps (__m128 __A, __m128 __B, __m128 __C)
{
  union128 a, b, c, e;
  a.x = __A;
  b.x = __B;
  c.x = __C;
  float d[4];
  int i;
  e.x = _mm_fmsubadd_ps (__A, __B, __C);
  for (i = 0; i < 4; i++)
    {
      d[i] = a.a[i] * b.a[i] + (i % 2 == 1 ? -c.a[i] : c.a[i]);
    }
  if (check_union128 (e, d))
    abort ();
}

void
check_mm_fmsubadd_pd (__m128d __A, __m128d __B, __m128d __C)
{
  union128d a, b, c, e;
  a.x = __A;
  b.x = __B;
  c.x = __C;
  double d[2];
  int i;
  e.x = _mm_fmsubadd_pd (__A, __B, __C);
  for (i = 0; i < 2; i++)
    {
      d[i] = a.a[i] * b.a[i] + (i % 2 == 1 ? -c.a[i] : c.a[i]);
    }
  if (check_union128d (e, d))
    abort ();
}

static void
fma_test (void)
{
  union128 a[3];
  union128d b[3];
  int i, j;
  for (i = 0; i < 3; i++)
    {
      for (j = 0; j < 4; j++)
	a[i].a[j] = i * j + 3.5;
      for (j = 0; j < 2; j++)
	b[i].a[j] = i * j + 3.5;
    }
  check_mm_fmsubadd_pd (b[0].x, b[1].x, b[2].x);
  check_mm_fmsubadd_ps (a[0].x, a[1].x, a[2].x);
}
