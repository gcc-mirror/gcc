/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (unsigned short *dst, unsigned char *src1, unsigned char *src2,
      int imm)
{
  int i, j, k, part, power;
  unsigned char tmp[2 * SIZE];;

  for (i = 0; i < 2 * SIZE; i += 16)
    {
      for (j = 0; j < 4; j++)
	{
	  power = 1;
	  for (k = 0; k < j; k++)
	    power *= 4;
	  part = (imm & (3 * power)) >> (2 * j);
	  for (k = 0; k < 4; k++)
	    tmp[i + 4 * j + k] = src2[i + 4 * part + k];
	}
    }

  for (i = 0; i < SIZE; i += 4)
    {
      dst[i] = dst[i + 1] = dst[i + 2] = dst[i + 3] = 0;
      for (j = 0; j < 4; j++)
	{
	  dst[i] += abs (src1[2 * i + j] - tmp[2 * i + j]);
	  dst[i + 1] += abs (src1[2 * i + j] - tmp[2 * i + j + 1]);
	  dst[i + 2] += abs (src1[2 * i + j + 4] - tmp[2 * i + j + 2]);
	  dst[i + 3] += abs (src1[2 * i + j + 4] - tmp[2 * i + j + 3]);
	}
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_w) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_b) src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  unsigned short res_ref[SIZE];
  int imm = 0x22;

  sign = -1;
  for (i = 0; i < 2*SIZE; i++)
    {
      src1.a[i] = 1 + 34 * i * sign;
      src1.a[i] = 179 - i;
      sign = sign * -1;
    }

  for (i = 0; i < SIZE; i++)
      res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_dbsad_epu8) (src1.x, src2.x, imm);
  res2.x = INTRINSIC (_mask_dbsad_epu8) (res2.x, mask, src1.x, src2.x, imm);
  res3.x = INTRINSIC (_maskz_dbsad_epu8) (mask, src1.x, src2.x, imm);

  CALC (res_ref, src1.a, src2.a, imm);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
