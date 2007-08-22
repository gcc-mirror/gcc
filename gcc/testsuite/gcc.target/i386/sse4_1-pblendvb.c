/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>
#include <string.h>

#define NUM 20

static void
init_pblendvb (unsigned char *src1, unsigned char *src2,
	       unsigned char *mask)
{
  int i, sign = 1; 

  for (i = 0; i < NUM * 16; i++)
    {
      src1[i] = i* i * sign;
      src2[i] = (i + 20) * sign;
      mask[i] = (i % 3) + ((i * (14 + sign))
			   ^ (src1[i] | src2[i] | (i*3)));
      sign = -sign;
    }
}

static int
check_pblendvb (__m128i *dst, unsigned char *src1,
		unsigned char *src2, unsigned char *mask)
{
  unsigned char tmp[16];
  int j;

  memcpy (&tmp[0], src1, sizeof (tmp));
  for (j = 0; j < 16; j++)
    if (mask [j] & 0x80)
      tmp[j] = src2[j];

  return memcmp (dst, &tmp[0], sizeof (tmp));
}

static void
sse4_1_test (void)
{
  union
    {
      __m128i x[NUM];
      unsigned char c[NUM * 16];
    } dst, src1, src2, mask;
  int i;

  init_pblendvb (src1.c, src2.c, mask.c);

  for (i = 0; i < NUM; i++)
    {
      dst.x[i] = _mm_blendv_epi8 (src1.x[i], src2.x[i], mask.x[i]);
      if (check_pblendvb (&dst.x[i], &src1.c[i * 16], &src2.c[i * 16],
			  &mask.c[i * 16]))
	abort ();
    }
}
