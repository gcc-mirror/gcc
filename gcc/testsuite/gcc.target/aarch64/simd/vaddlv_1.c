/* { dg-do compile } */
/* { dg-options "-O2" } */


#include <arm_neon.h>

#define FUNC(IT, OT, S)	\
OT			\
foo_##S (IT a)		\
{			\
  return vaddlv_##S (a);\
}

FUNC (int8x8_t, int16_t, s8)
/* { dg-final { scan-assembler-times {saddlv\th[0-9]+, v0\.8b} 1} }  */

FUNC (int16x4_t, int32_t, s16)
/* { dg-final { scan-assembler-times {saddlv\ts[0-9]+, v0\.4h} 1} }  */

FUNC (int32x2_t, int64_t, s32)
/* { dg-final { scan-assembler-times {saddlp\tv[0-9]+\.1d, v0\.2s} 1} }  */

FUNC (uint8x8_t, uint16_t, u8)
/* { dg-final { scan-assembler-times {uaddlv\th[0-9]+, v0\.8b} 1} }  */

FUNC (uint16x4_t, uint32_t, u16)
/* { dg-final { scan-assembler-times {uaddlv\ts[0-9]+, v0\.4h} 1} }  */

FUNC (uint32x2_t, uint64_t, u32)
/* { dg-final { scan-assembler-times {uaddlp\tv[0-9]+\.1d, v0\.2s} 1} }  */

#define FUNCQ(IT, OT, S)	\
OT				\
fooq_##S (IT a)			\
{				\
  return vaddlvq_##S (a);	\
}

FUNCQ (int8x16_t, int16_t, s8)
/* { dg-final { scan-assembler-times {saddlv\th[0-9]+, v0\.16b} 1} }  */

FUNCQ (int16x8_t, int32_t, s16)
/* { dg-final { scan-assembler-times {saddlv\ts[0-9]+, v0\.8h} 1} }  */

FUNCQ (int32x4_t, int64_t, s32)
/* { dg-final { scan-assembler-times {saddlv\td[0-9]+, v0\.4s} 1} }  */

FUNCQ (uint8x16_t, uint16_t, u8)
/* { dg-final { scan-assembler-times {uaddlv\th[0-9]+, v0\.16b} 1} }  */

FUNCQ (uint16x8_t, uint32_t, u16)
/* { dg-final { scan-assembler-times {uaddlv\ts[0-9]+, v0\.8h} 1} }  */

FUNCQ (uint32x4_t, uint64_t, u32)
/* { dg-final { scan-assembler-times {uaddlv\td[0-9]+, v0\.4s} 1} }  */

