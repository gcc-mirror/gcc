/* { dg-do compile } */
/* { dg-require-effective-target arm_simd32_ok } */
/* { dg-add-options arm_simd32 } */

#include <arm_acle.h>

int8x4_t
test_sel (int8x4_t a, int8x4_t b, uint8x4_t c, uint8x4_t d)
{
  int8x4_t res1 = __sadd8 (a, b);
  return __sel (c, d);
}

/* { dg-final { scan-assembler-times "sadd8\t...?, ...?, ...?" 1 } } */
/* { dg-final { scan-assembler-times "sel\t...?, ...?, ...?" 1 } } */
