/* PR target/51968 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=armv7-a -mfloat-abi=softfp -mfpu=neon" } */
/* { dg-require-effective-target arm_neon_ok } */
#include <arm_neon.h>

struct T { int8x8x2_t val; };
int y;

void
foo (int8x8_t z, int8x8_t x, int16x8_t b, int8x8_t n)
{
  if (y)
    {
      struct T m;
      m.val = vuzp_s8 (z, x);
    }
  for (;;)
    {
      int8x16_t g;
      int8x8_t h, j, k;
      struct T m;
      j = vqmovn_s16 (b);
      g = vcombine_s8 (j, h);
      k = vget_low_s8 (g);
      m.val = vuzp_s8 (k, n);
    }
}
