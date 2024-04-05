/* PR tree-optimization/114566 */
/* { dg-do run } */
/* { dg-options "-O3 -mavx512f" } */
/* { dg-additional-options "-fstack-protector-strong" { target fstack_protector } } */
/* { dg-require-effective-target avx512f } */

#define AVX512F
#include "avx512f-helper.h"

__attribute__((noipa)) int
foo (float x, float y)
{
  float a[8][56];
  __builtin_memset (a, 0, sizeof (a));

  for (int j = 0; j < 8; j++)
    for (int k = 0; k < 56; k++)
      {
	float b = k * y;
	if (b < 0.)
	  b = 0.;
	if (b > 0.)
	  b = 0.;
	a[j][k] += b;
      }

  return __builtin_log (x);
}

void
TEST (void)
{
  foo (86.25f, 0.625f);
}
