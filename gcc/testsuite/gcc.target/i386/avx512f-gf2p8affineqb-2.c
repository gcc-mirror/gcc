/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mgfni -mavx512bw" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target gfni } */

#define AVX512F

#define GFNI
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)

#include "avx512f-mask-type.h"
#include <x86intrin.h>

static void
CALC (unsigned char *r, unsigned char *s1, unsigned char *s2, unsigned char imm)
{
  for (int a = 0; a < SIZE/8; a++)
    {
      for (int val = 0; val < 8; val++)
        {
          unsigned char result = 0;
          for (int bit = 0; bit < 8; bit++)
          {
            unsigned char temp = s1[a*8 + val] & s2[a*8 + bit];
            unsigned char parity = __popcntd(temp);
            if (parity % 2)
              result |= (1 << (8 - bit - 1));
          }
          r[a*8 + val] = result ^ imm; 
        }
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_b) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  char res_ref[SIZE];
  unsigned char imm = 0;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 1 + i;
      src2.a[i] = 1;
    }

  for (i = 0; i < SIZE; i++)
    {
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  CALC (res_ref, src1.a, src2.a, imm);

  res1.x = INTRINSIC (_gf2p8affine_epi64_epi8) (src1.x, src2.x, imm);
  res2.x = INTRINSIC (_mask_gf2p8affine_epi64_epi8) (res2.x, mask, src1.x, src2.x, imm);
  res3.x = INTRINSIC (_maskz_gf2p8affine_epi64_epi8) (mask, src1.x, src2.x, imm);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, res_ref))
    abort ();

}
