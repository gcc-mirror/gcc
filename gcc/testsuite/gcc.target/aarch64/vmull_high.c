/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

#include <arm_neon.h>

int64x2_t
doit (int8x16_t a)
{
  int16x8_t b = vmull_high_s8 (a, a);
  int32x4_t c = vmull_high_s16 (b, b);
  return vmull_high_s32 (c, c);
}

uint64x2_t
douit (uint8x16_t a)
{
  uint16x8_t b = vmull_high_u8 (a, a);
  uint32x4_t c = vmull_high_u16 (b, b);
  return vmull_high_u32 (c, c);
}

/* { dg-final { scan-assembler-times "smull2\[ |\t\]*v" 3} }  */
/* { dg-final { scan-assembler-times "umull2\[ |\t\]*v" 3} }  */
