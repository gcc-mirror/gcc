/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#include <string.h>

#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

#define N 0x3

void
CALC (char *src1, char *src2, char * dst)
{
  /* result for EVEX.U1.512 version consists from 4 result block, each of them
   * has length of 128 bits. */
  unsigned block_len = 16;
  unsigned double_block_len = 32;
  unsigned shift = 0;
  char buf[double_block_len];
  char *bout = dst;
  int bits, i;

  for (bits = 0; bits < AVX512F_LEN; bits += 128)
    {
      memcpy (&buf[0], src2 + shift, block_len);
      memcpy (&buf[block_len], src1 + shift, block_len);

      for (i = 0; i < block_len; i++)
	/* shift counts larger than 32 produces zero result. */
	if (N >= 32 || N + i >= 32)
	  bout[i] = 0;
	else
	  bout[i] = buf[N + i];

      shift += block_len;
      bout += block_len;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_b) s1, s2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  char res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i;
      s2.a[i] = i * 2;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_alignr_epi8) (s1.x, s2.x, N);
  res2.x = INTRINSIC (_mask_alignr_epi8) (res2.x, mask, s1.x, s2.x, N);
  res3.x = INTRINSIC (_maskz_alignr_epi8) (mask, s1.x, s2.x, N);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, res_ref))
    abort ();
}
