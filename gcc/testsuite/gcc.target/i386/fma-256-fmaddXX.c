/* { dg-do run } */
/* { dg-require-effective-target fma } */
/* { dg-options "-O2 -mfma" } */

#include "fma-check.h"

#include <x86intrin.h>
#include "m256-check.h"

void
check_mm256_fmadd_pd (__m256d __A, __m256d __B, __m256d __C)
{
  union256d a, b, c, e;
  a.x = __A;
  b.x = __B;
  c.x = __C;
  double d[4];
  int i;
  e.x = _mm256_fmadd_pd (__A, __B, __C);
  for (i = 0; i < 4; i++)
    {
      d[i] = a.a[i] * b.a[i] + c.a[i];
    }
  if (check_union256d (e, d))
    abort ();
}

void
check_mm256_fmadd_ps (__m256 __A, __m256 __B, __m256 __C)
{
  union256 a, b, c, e;
  a.x = __A;
  b.x = __B;
  c.x = __C;
  float d[8];
  int i;
  e.x = _mm256_fmadd_ps (__A, __B, __C);
  for (i = 0; i < 8; i++)
    {
      d[i] = a.a[i] * b.a[i] + c.a[i];
    }
  if (check_union256 (e, d))
    abort ();
}

static void
fma_test (void)
{
  union256 c[3];
  union256d d[3];
  int i, j;
  for (i = 0; i < 3; i++)
    {
      for (j = 0; j < 8; j++)
	c[i].a[j] = i * j + 3.5;
      for (j = 0; j < 4; j++)
	d[i].a[j] = i * j + 3.5;
    }
  check_mm256_fmadd_pd (d[0].x, d[1].x, d[2].x);
  check_mm256_fmadd_ps (c[0].x, c[1].x, c[2].x);
}
