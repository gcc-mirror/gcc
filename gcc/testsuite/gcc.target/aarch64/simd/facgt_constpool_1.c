/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

uint32x4_t g (uint32x4_t, uint32x4_t);

uint32x4_t
foo (float32x4_t x, float32x4_t a, float32x4_t b)
{
  return vcagtq_f32 (x, (float32x4_t){ 100.0, 100.0, 100.0, 100.0});
}

/* { dg-final { scan-assembler-times {facgt\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s} 1 } } */
/* { dg-final { scan-assembler-not {\tfcmgt\t} } } */

