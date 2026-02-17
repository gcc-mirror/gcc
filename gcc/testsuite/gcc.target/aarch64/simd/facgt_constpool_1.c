/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

uint32x4_t
foo (float32x4_t x)
{
  return vcagtq_f32 (x, (float32x4_t) {100.0, 100.0, 100.0, 100.0});
}

uint32x4_t
bar (float32x4_t x)
{
  return vcagtq_f32 ((float32x4_t) {100.0, 100.0, 100.0, 100.0}, x);
}

/* FIXME(Karl Meakin): Recognise more complex constants that have to be loaded
   from constant pools as well (eg {100.0, 200.0, 300.0, 400.0}).  */

/* { dg-final { scan-assembler-times {facgt\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s} 2 } } */
/* { dg-final { scan-assembler-not {\tfcmgt\t} } } */
