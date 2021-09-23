/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64 -mfpmath=sse" } */

#include <stdint.h>

#define loop_t uint32_t
#define idx_t uint32_t

void loop(float * const __restrict__ dst,
          float const * const __restrict__ src,
          idx_t const * const __restrict__ idx,
          loop_t const begin,
          loop_t const end)
{
  for (loop_t i = begin; i < end; ++i)
    dst[i] = 42.0 * src[idx[i]];
}

/* { dg-final { scan-assembler-times "mulps" 1 } } */
