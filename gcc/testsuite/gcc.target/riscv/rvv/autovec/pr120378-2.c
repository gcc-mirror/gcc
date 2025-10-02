/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include <stdint.h>

inline uint16_t
clip_uint16 (int x)
{
  return x & (~65535) ? (-x) >> 31 : x;
}

void __attribute__ ((noipa))
clip_loop (uint16_t *res, int *x, int w)
{
  for (int i = 0; i < w; i++)
    res[i] = clip_uint16 (x[i]);
}

/* { dg-final { scan-tree-dump-times ".SAT_TRUNC " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR " 1 "optimized" } } */
/* { dg-final { scan-assembler-times {vnclipu\.wi} 1 } } */
