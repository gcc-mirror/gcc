/* { dg-do run } */
/* { dg-options "-O2 -mvsx -Wno-psabi" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1
#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>
#include <string.h>

#define NUM 20

#ifndef MASK
#define MASK 0x0f
#endif

static void
init_pblendw (short *src1, short *src2)
{
  int i, sign = 1;

  for (i = 0; i < NUM * 8; i++)
    {
      src1[i] = i * i * sign;
      src2[i] = (i + 20) * sign;
      sign = -sign;
    }
}

static int
check_pblendw (__m128i *dst, short *src1, short *src2)
{
  short tmp[8];
  int j;

  memcpy (&tmp[0], src1, sizeof (tmp));
  for (j = 0; j < 8; j++)
    if ((MASK & (1 << j)))
      tmp[j] = src2[j];

  return memcmp (dst, &tmp[0], sizeof (tmp));
}

static void
TEST (void)
{
  __m128i x, y;
  union
    {
      __m128i x[NUM];
      short s[NUM * 8];
    } dst, src1, src2;
  union
    {
      __m128i x;
      short s[8];
    } src3;
  int i;

  init_pblendw (src1.s, src2.s);

  /* Check pblendw imm8, m128, xmm */
  for (i = 0; i < NUM; i++)
    {
      dst.x[i] = _mm_blend_epi16 (src1.x[i], src2.x[i], MASK); 
      if (check_pblendw (&dst.x[i], &src1.s[i * 8], &src2.s[i * 8]))
	abort ();
    }
    
   /* Check pblendw imm8, xmm, xmm */
  src3.x = _mm_setzero_si128 ();

  x = _mm_blend_epi16 (dst.x[2], src3.x, MASK);
  y = _mm_blend_epi16 (src3.x, dst.x[2], MASK);

  if (check_pblendw (&x, &dst.s[16], &src3.s[0]))
    abort ();

  if (check_pblendw (&y, &src3.s[0], &dst.s[16]))
    abort ();
}
