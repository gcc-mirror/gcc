/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable -mrvv-max-lmul=m1 -fno-vect-cost-model -fdump-tree-vect-details" } */

__attribute__ ((noipa)) void
test_strided_concat (int *restrict dst, const int *restrict src, long stride)
{
#pragma GCC unroll 1
  for (int i = 0; i < 32; ++i)
    {
      dst[2 * i] = src[i * stride];
      dst[2 * i + 1] = src[i * stride + 1];
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-assembler-times "vslideup" 1 } } */
