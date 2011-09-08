/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define msk0 0xC0
#define msk1 0x01
#define msk2 0xF2
#define msk3 0x03
#define msk4 0x84
#define msk5 0x05
#define msk6 0xE6
#define msk7 0x67


static void
compute_mpsadbw (int *i1, int *i2, int mask, int *r)
{
  unsigned char s[4];
  int i, j;
  int offs1, offs2;
  unsigned char *v1 = (char *) i1;
  unsigned char *v2 = (char *) i2;
  unsigned short *ret = (unsigned short *) r;

  memset (ret, 0, 32);

  /* Lower part */
  offs2 = 4 * (mask & 3);
  for (i = 0; i < 4; i++)
    s[i] = v2[offs2 + i];

  offs1 = 4 * ((mask & 4) >> 2);
  for (j = 0; j < 8; j++)
    for (i = 0; i < 4; i++)
      ret[j] += abs (v1[offs1 + j + i] - s[i]);

  /* Higher part */
  offs2 = 4 * ((mask >> 3) & 3) + 16;
  for (i = 0; i < 4; i++)
    s[i] = v2[offs2 + i];

  offs1 = 4 * ((mask & 0x20) >> 5) + 16;
  for (j = 0; j < 8; j++)
    for (i = 0; i < 4; i++)
      ret[j + 8] += abs (v1[offs1 + j + i] - s[i]);
}

static void
avx2_test (void)
{
  union256i_d val1, val2, val3[8], res[8];
  int tmp[8];
  unsigned char masks[8];
  int i, j;

  val1.a[0] = 0x35251505;
  val1.a[1] = 0x75655545;
  val1.a[2] = 0xB5A59585;
  val1.a[3] = 0xF5E5D5C5;

  val1.a[4] = 0x35251505;
  val1.a[5] = 0x75655545;
  val1.a[6] = 0xB5A59585;
  val1.a[7] = 0xF5E5D5C5;

  val2.a[0] = 0x31211101;
  val2.a[1] = 0x71615141;
  val2.a[2] = 0xB1A19181;
  val2.a[3] = 0xF1E1D1C1;

  val2.a[4] = 0x31211101;
  val2.a[5] = 0x71615141;
  val2.a[6] = 0xB1A19181;
  val2.a[7] = 0xF1E1D1C1;

  for (i = 0; i < 8; i++)
    switch (i % 3)
      {
      case 1:
	val3[i].a[0] = 0xF1E1D1C1;
	val3[i].a[1] = 0xB1A19181;
	val3[i].a[2] = 0x71615141;
	val3[i].a[3] = 0x31211101;
	break;
      default:
	val3[i].x = val2.x;
	break;
      }

  /* Check mpsadbw imm8, ymm, ymm.  */
  res[0].x = _mm256_mpsadbw_epu8 (val1.x, val2.x, msk0);
  res[1].x = _mm256_mpsadbw_epu8 (val1.x, val2.x, msk1);
  res[2].x = _mm256_mpsadbw_epu8 (val1.x, val2.x, msk2);
  res[3].x = _mm256_mpsadbw_epu8 (val1.x, val2.x, msk3);
  res[4].x = _mm256_mpsadbw_epu8 (val1.x, val2.x, msk4);
  res[5].x = _mm256_mpsadbw_epu8 (val1.x, val2.x, msk5);
  res[6].x = _mm256_mpsadbw_epu8 (val1.x, val2.x, msk6);
  res[7].x = _mm256_mpsadbw_epu8 (val1.x, val2.x, msk7);

  masks[0] = msk0;
  masks[1] = msk1;
  masks[2] = msk2;
  masks[3] = msk3;
  masks[4] = msk4;
  masks[5] = msk5;
  masks[6] = msk6;
  masks[7] = msk7;

  for (i = 0; i < 8; i++)
    {
      compute_mpsadbw (val1.a, val2.a, masks[i], tmp);
      if (check_union256i_d (res[i], tmp))
	abort ();
    }

  /* Check mpsadbw imm8, m256, ymm.  */
  for (i = 0; i < 8; i++)
    {
      res[i].x = _mm256_mpsadbw_epu8 (val1.x, val3[i].x, msk4);
      masks[i] = msk4;
    }

  for (i = 0; i < 8; i++)
    {
      compute_mpsadbw (val1.a, val3[i].a, masks[i], tmp);
      if (check_union256i_d (res[i], tmp))
	abort ();
    }
}
