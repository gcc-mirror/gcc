/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include <stdint.h>

inline uint8_t
clip_uint8 (int64_t x)
{
  return x & (~255) ? (-x) >> 63 : x;
}

void __attribute__ ((noipa))
clip_loop (uint8_t *res, int64_t *x, int w)
{
  for (int i = 0; i < w; i++)
    res[i] = clip_uint8 (x[i]);
}

/* { dg-final { scan-tree-dump-times ".SAT_TRUNC " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR " 1 "optimized" } } */
/* { dg-final { scan-assembler-times {vnclipu\.wi} 3 } } */
