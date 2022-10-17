/* { dg-do compile } */
/* { dg-options "-march=cannonlake -O2" } */
/* { dg-final { scan-assembler-not {(?n)vmovdq} } } */
/* There should be no load + vpcmp, just vpcmp with memory operand.  */

#include<immintrin.h>
__mmask16
foo (__m256i* n, __m256i mch256)
{
  __m256i data1 = _mm256_loadu_si256(n);
  __m256i data2 = _mm256_loadu_si256(n+1);
  __mmask16 mask1 = _mm256_cmpeq_epu16_mask(data1, mch256);
  __mmask16 mask2 = _mm256_cmpeq_epu16_mask(data2, mch256);
  return mask1 + mask2;
}

__mmask16
foo1 (__m256i* n, __m256i mch256)
{
  __m256i data1 = _mm256_loadu_si256(n);
  __m256i data2 = _mm256_loadu_si256(n+1);
  __mmask16 mask1 = _mm256_cmpeq_epu16_mask(mch256, data1);
  __mmask16 mask2 = _mm256_cmpeq_epu16_mask(mch256, data2);
  return mask1 + mask2;
}
