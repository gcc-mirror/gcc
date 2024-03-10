/* { dg-do run } */
/* { dg-options "-O2 -mavx512bitalg" } */
/* { dg-require-effective-target avx512bitalg } */

#define AVX512BITALG
#define SIZE (AVX512F_LEN / 8)

#include "avx512f-helper.h"
#include "avx512f-mask-type.h"

#define TYPE unsigned long long

unsigned char
CALC (TYPE a, TYPE b)
{
 unsigned char res = 0;
 for (int i = 0; i < 8; i++)
  {
    unsigned char m = (b >> (64 - ((i+1)*8))) & 0x3F;
    unsigned char bit = (a >> m) & 1;
    res |= (bit << (8 - i - 1));
  }

 return res;
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  TYPE res1, res2;
  TYPE res_ref = 0;

  src1.x = INTRINSIC (_set1_epi8) (0x13);
  src2.x = INTRINSIC (_set1_epi8) (0x17);

  src1.a[0] = 0xff;
  src2.a[0] = 0xff;

  for (int i = 0; i < SIZE/8; i++)
  {
    unsigned long long bit = CALC (src1.a[i], src2.a[i]);
    res_ref |= ((unsigned long long)(CALC (src1.a[i], src2.a[i])) << (i*8));
  }

  res1 = INTRINSIC (_bitshuffle_epi64_mask)      (src1.x, src2.x);
  res2 = INTRINSIC (_mask_bitshuffle_epi64_mask) (mask, src1.x, src2.x);
 
  if (res1 != res_ref)
    abort();

  for (int i = 0; i < SIZE; i++)
  {
    if (!((mask >> i) & 1))
      res_ref &= ~((unsigned long long)1 <<i);
  }
  if (res2 != res_ref)
    abort();
}
