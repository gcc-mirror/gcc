/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-O2 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include "sse4_1-check.h"

#include <smmintrin.h>
#include <string.h>
#include <stdlib.h>

#define NUM 20

#undef MASK
#define MASK 0xe

static void
init_blendps (float *src1, float *src2)
{
  int i, sign = 1;

  for (i = 0; i < NUM * 4; i++)
    {
      src1[i] = i * i * sign;
      src2[i] = (i + 20) * sign;
      sign = -sign;
    }
}

static int
check_blendps (__m128 *dst, float *src1, float *src2)
{
  float tmp[4];
  int j;

  memcpy (&tmp[0], src1, sizeof (tmp));
  for (j = 0; j < 4; j++)
    if ((MASK & (1 << j)))
      tmp[j] = src2[j];

  return memcmp (dst, &tmp[0], sizeof (tmp));
}

static void
sse4_1_test (void)
{
  __m128 x, y;
  union
    {
      __m128 x[NUM];
      float f[NUM * 4];
    } dst, src1, src2;
  union
    {
      __m128 x;
      float f[4];
    } src3;
  int i;

  init_blendps (src1.f, src2.f);

  for (i = 0; i < 4; i++)
    src3.f[i] = (int) rand ();

  /* Check blendps imm8, m128, xmm */
  for (i = 0; i < NUM; i++)
    {
      dst.x[i] = _mm_blend_ps (src1.x[i], src2.x[i], MASK); 
      if (check_blendps (&dst.x[i], &src1.f[i * 4], &src2.f[i * 4]))
	abort ();
    }
    
   /* Check blendps imm8, xmm, xmm */
  x = _mm_blend_ps (dst.x[2], src3.x, MASK);
  y = _mm_blend_ps (src3.x, dst.x[2], MASK);

  if (check_blendps (&x, &dst.f[8], &src3.f[0]))
    abort ();

  if (check_blendps (&y, &src3.f[0], &dst.f[8]))
    abort ();
}
