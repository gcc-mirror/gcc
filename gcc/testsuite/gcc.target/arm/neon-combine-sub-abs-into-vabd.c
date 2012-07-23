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

#include <arm_neon.h>
int8x8_t sub_abs_to_vabd_8(int8x8_t val1, int8x8_t val2)
{
  int8x8_t sres = vsub_s8(val1, val2);
  int8x8_t res = vabs_s8 (sres);

  return res;
}
/* { dg-final { scan-assembler "vabd\.s8" } }*/

int16x4_t sub_abs_to_vabd_16(int16x4_t val1, int16x4_t val2)
{
  int16x4_t sres = vsub_s16(val1, val2);
  int16x4_t res = vabs_s16 (sres);

  return res;
}
/* { dg-final { scan-assembler "vabd\.s16" } }*/

int32x2_t sub_abs_to_vabd_32(int32x2_t val1, int32x2_t val2)
{
  int32x2_t sres = vsub_s32(val1, val2);
  int32x2_t res = vabs_s32 (sres);

   return res;
}
/* { dg-final { scan-assembler "vabd\.s32" } }*/
