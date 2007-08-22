/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#include <smmintrin.h>
#include <string.h>

#define msk0 0x01
#define msk1 0x10
#define msk2 0x29
#define msk3 0x30

#define msk4 0xFC
#define msk5 0x05
#define msk6 0x0A
#define msk7 0x0F

static void
sse4_1_test (void)
{
  union
    {
      __m128 x;
      float f[4];
    } res[8], val1, val2, tmp;
  int masks[8];
  int i, j;

  val2.f[0] = 55.0;
  val2.f[1] = 55.0;
  val2.f[2] = 55.0;
  val2.f[3] = 55.0;

  val1.f[0] = 1.;
  val1.f[1] = 2.;
  val1.f[2] = 3.;
  val1.f[3] = 4.;

  res[0].x = _mm_insert_ps (val2.x, val1.x, msk0);
  res[1].x = _mm_insert_ps (val2.x, val1.x, msk1);
  res[2].x = _mm_insert_ps (val2.x, val1.x, msk2);
  res[3].x = _mm_insert_ps (val2.x, val1.x, msk3);

  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;

  for (i = 0; i < 4; i++)
    res[i + 4].x = _mm_insert_ps (val2.x, val1.x, msk4);

  masks[4] = msk4;
  masks[5] = msk4;
  masks[6] = msk4;
  masks[7] = msk4;

  for (i=0; i < 8; i++)
    {
      tmp = val2;
      tmp.f[(masks[i] & 0x30) >> 4] = val1.f[(masks[i] & 0xC0) >> 6];

      for (j = 0; j < 4; j++)
	if (masks[i] & (0x1 << j))
	  tmp.f[j] = 0.f;

      if (memcmp (&res[i], &tmp, sizeof (tmp)))
	abort ();
    }
} 
