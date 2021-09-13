/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

#include <arm_neon.h>

#define FUNC(IT, OT, S)         \
OT                              \
foo_##S (IT a)                  \
{                               \
  return vmovl_high_##S (a);    \
}

FUNC (int8x16_t, int16x8_t, s8)
/* { dg-final { scan-assembler-times {sxtl2\tv0\.8h, v0\.16b} 1} }  */

FUNC (int16x8_t, int32x4_t, s16)
/* { dg-final { scan-assembler-times {sxtl2\tv0\.4s, v0\.8h} 1} }  */

FUNC (int32x4_t, int64x2_t, s32)
/* { dg-final { scan-assembler-times {sxtl2\tv0\.2d, v0\.4s} 1} }  */

FUNC (uint8x16_t, uint16x8_t, u8)
/* { dg-final { scan-assembler-times {uxtl2\tv0\.8h, v0\.16b} 1} }  */

FUNC (uint16x8_t, uint32x4_t, u16)
/* { dg-final { scan-assembler-times {uxtl2\tv0\.4s, v0\.8h} 1} }  */

FUNC (uint32x4_t, uint64x2_t, u32)
/* { dg-final { scan-assembler-times {uxtl2\tv0\.2d, v0\.4s} 1} }  */

