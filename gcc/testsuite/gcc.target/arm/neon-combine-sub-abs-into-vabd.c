/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 -funsafe-math-optimizations" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>
float32x2_t f_sub_abs_to_vabd_32(float32x2_t val1, float32x2_t val2)
{
  float32x2_t sres = vsub_f32(val1, val2);
  float32x2_t res = vabs_f32 (sres);

  return res;
}
/* { dg-final { scan-assembler "vabd\.f32" } }*/
