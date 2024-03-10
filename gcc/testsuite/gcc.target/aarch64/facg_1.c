/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

int g(uint32x4_t, uint32x4_t);

int foo (float32x4_t x, float32x4_t a, float32x4_t b)
{
  return g(vcagtq_f32 (x, a), vcagtq_f32 (x, b));
}

/* { dg-final { scan-assembler-times {facgt\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s} 2 } } */
/* { dg-final { scan-assembler-not {\tfabs\t} } } */

