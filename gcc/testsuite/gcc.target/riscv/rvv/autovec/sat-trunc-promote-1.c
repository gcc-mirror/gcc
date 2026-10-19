/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -mrvv-max-lmul=m1 -fno-vect-cost-model -fdump-tree-optimized" } */

#include <stdint-gcc.h>

__attribute__ ((noipa)) void
clip_scale_u8 (uint8_t *__restrict dst, const int8_t *__restrict src, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int x = src[i] * 3;
      dst[i] = (unsigned) x > 255u ? (int) (-(unsigned) x) >> 31 : x;
    }
}

/* { dg-final { scan-tree-dump-times {\.SAT_TRUNC } 1 "optimized" } } */
/* { dg-final { scan-assembler-times {\tvnclipu\.wi} 1 } } */
