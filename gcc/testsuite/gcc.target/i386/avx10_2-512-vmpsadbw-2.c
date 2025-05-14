/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif
#include "avx10-helper.h"

#define SRC_SIZE (AVX512F_LEN / 8)
#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

static void
CALC (short* dst, char* src1, char* src2, int cont)
{
  int blk2_pos, blk1_pos, i, j, k, c;
  char blk1[12], blk2[4], x;
  short tmp[4], s;
  
  for (k = 0; k < AVX512F_LEN / 128; k++)
  {
    c = cont & 0xff;
    if (k % 2 == 1)
      c >>= 3;
    blk2_pos = (c & 3) * 4;
    blk1_pos = ((c >> 2) & 1) * 4;

    for (i = 0; i < 11; i++)
      blk1[i] = src1[16 * k + i + blk1_pos];

    for (i = 0; i < 4; i++)
      blk2[i] = src2[16 * k + i + blk2_pos];

    for (i = 0; i < 8; i++)
      {
	for (j = 0; j < 4; j++)
	  {
	    x = blk1[j + i] - blk2[j];
	    tmp[j] = x > 0 ? x : -x;
	  }

	s = 0;
	for (j = 0; j < 4; j++)
	  s += tmp[j];
	dst[8 * k + i] = s;
      }
  }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_w) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_b) src1;
  UNION_TYPE (AVX512F_LEN, i_b) src2;
  MASK_TYPE mask = MASK_VALUE;
  short res_ref[SIZE], res_ref2[SIZE];

  for (i = 0; i < SRC_SIZE; i++)
    {
      src1.a[i] = 10 + 2 * i;
      src2.a[i] = 3 * i;
    }

  for (i = 0; i < SIZE; i++)
    {
      res1.a[i] = 0x7FFF;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  CALC (res_ref, src1.a, src2.a, 0x21);
  CALC (res_ref2, src1.a, src2.a, 0x21);

  res1.x = INTRINSIC (_mpsadbw_epu8) (src1.x, src2.x, 0x21);
  res2.x = INTRINSIC (_mask_mpsadbw_epu8) (res2.x, mask, src1.x, src2.x, 0x21);
  res3.x = INTRINSIC (_maskz_mpsadbw_epu8) (mask, src1.x, src2.x, 0x21);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref2))
    abort ();

  MASK_ZERO (i_w) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref2))
    abort ();
}
