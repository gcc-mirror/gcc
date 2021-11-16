/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpternlog" 5 } } */
/* { dg-final { scan-assembler-not "vpxor" } } */
/* { dg-final { scan-assembler-not "vpor" } } */
/* { dg-final { scan-assembler-not "vpand" } } */

#include<immintrin.h>

extern __m256i src1, src2, src3;

__m256i
foo (void)
{
  return (src2 & ~src1) | (src3 & src1);
}

__m256i
foo1 (void)
{
  return (src2 & src1) | (src3 & ~src1);
}

__m256i
foo2 (void)
{
  return (src2 & src1) | (~src3 & src1);
}

__m256i
foo3 (void)
{
  return (~src2 & src1) | (src3 & src1);
}

__m256i
foo4 (void)
{
  return src3 & src2 ^ src1;
}
