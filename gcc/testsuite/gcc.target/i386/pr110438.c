/* { dg-do compile } */
/* { dg-options "-mavx512f -O2 -ftree-vectorize -mno-avx512dq -dp -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times {cvtmask2.*_pternlog} "1" } } */
/* { dg-final { scan-assembler-times {constm1_pternlog} "1" } } */
/* { dg-final { scan-assembler-not {(?n)vpternlogd.*\(} } } */


#include <immintrin.h>

__m512i g(void)
{
  return (__m512i){ 0 } - 1;
}

__m512i g1(__m512i* a)
{
  return ~(*a);
}

void
foo (int* a, int* __restrict b)
{
  for (int i = 0; i != 16; i++)
    {
      if (b[i])
	a[i] = -1;
      else
	a[i] = 0;
    }
}
