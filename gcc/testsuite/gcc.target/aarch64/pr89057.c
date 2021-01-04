/* { dg-options "-O3" } */

#include <arm_neon.h>

void
f (int32_t *dst, int32_t *src, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32x2x3_t a = vld3_s32 (src + i * 6);
      int32x2x3_t b = { a.val[2], a.val[1], a.val[0] };
      vst3_s32 (dst + i * 6, b);
    }
}

/* { dg-final { scan-assembler-not {\tins\t} } } */
