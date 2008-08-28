/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

#include <smmintrin.h>
#include <string.h>

#define msk0 0xC0
#define msk1 0x01
#define msk2 0xF2
#define msk3 0x03
#define msk4 0x84
#define msk5 0x05
#define msk6 0xE6
#define msk7 0x67

static __m128i
compute_mpsadbw (unsigned char *v1, unsigned char *v2, int mask)
{
  union
    {
      __m128i x;
      unsigned short s[8];
    } ret;
  unsigned char s[4];
  int i, j;
  int offs1, offs2;

  offs2 = 4 * (mask & 3);
  for (i = 0; i < 4; i++)
    s[i] = v2[offs2 + i];

  offs1 = 4 * ((mask & 4) >> 2);
  for (j = 0; j < 8; j++)
    {
      ret.s[j] = 0;
      for (i = 0; i < 4; i++)
	ret.s[j] += abs (v1[offs1 + j + i] - s[i]);
    }

  return ret.x;
}

static void
TEST (void)
{
  union
    {
      __m128i x;
      unsigned int i[4];
      unsigned char c[16];
    } val1, val2, val3 [8];
  __m128i res[8], tmp;
  unsigned char masks[8];
  int i;

  val1.i[0] = 0x35251505;
  val1.i[1] = 0x75655545;
  val1.i[2] = 0xB5A59585;
  val1.i[3] = 0xF5E5D5C5;

  val2.i[0] = 0x31211101;
  val2.i[1] = 0x71615141;
  val2.i[2] = 0xB1A19181;
  val2.i[3] = 0xF1E1D1C1;

  for (i=0; i < 8; i++)
    switch (i % 3)
      {
      case 1:
	val3[i].i[0] = 0xF1E1D1C1;
	val3[i].i[1] = 0xB1A19181;
	val3[i].i[2] = 0x71615141;
	val3[i].i[3] = 0x31211101;
	break;
      default:
	val3[i].x = val2.x;
	break;
      }

  /* Check mpsadbw imm8, xmm, xmm.  */
  res[0] = _mm_mpsadbw_epu8 (val1.x, val2.x, msk0);
  res[1] = _mm_mpsadbw_epu8 (val1.x, val2.x, msk1);
  res[2] = _mm_mpsadbw_epu8 (val1.x, val2.x, msk2);
  res[3] = _mm_mpsadbw_epu8 (val1.x, val2.x, msk3);
  res[4] = _mm_mpsadbw_epu8 (val1.x, val2.x, msk4);
  res[5] = _mm_mpsadbw_epu8 (val1.x, val2.x, msk5);
  res[6] = _mm_mpsadbw_epu8 (val1.x, val2.x, msk6);
  res[7] = _mm_mpsadbw_epu8 (val1.x, val2.x, msk7);

  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;
  masks[4] = msk4;
  masks[5] = msk5;
  masks[6] = msk6;
  masks[7] = msk7;

  for (i=0; i < 8; i++)
    {
      tmp = compute_mpsadbw (val1.c, val2.c, masks[i]);
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }
    
  /* Check mpsadbw imm8, m128, xmm.  */
  for (i=0; i < 8; i++)
    {
      res[i] = _mm_mpsadbw_epu8 (val1.x, val3[i].x, msk4);
      masks[i] = msk4;
    }

  for (i=0; i < 8; i++)
    {
      tmp = compute_mpsadbw (val1.c, val3[i].c, masks[i]);
      if (memcmp (&tmp, &res[i], sizeof (tmp)))
	abort ();
    }
}
