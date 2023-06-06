/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

#define MYOP(OT,IT,IMT,OP,IS,OS)                         \
OT                                              \
foo_##OP##_##OS (IT a)                     \
{                                               \
  IMT zeros = vcreate_##OS (0);                   \
  return vcombine_##OS (v##OP##_##IS (a, 3), zeros);      \
}

#define FUNC(OT,IT,IMT,IS,OS)			\
MYOP (OT, IT, IMT, qshrn_n, IS, OS)		\
MYOP (OT, IT, IMT, qrshrn_n, IS, OS)		\
MYOP (OT, IT, IMT, shrn_n, IS, OS)		\
MYOP (OT, IT, IMT, rshrn_n, IS, OS)

#define FUNCUN(OT,IT,IMT,IS,OS)			\
MYOP (OT, IT, IMT, qshrun_n, IS, OS)		\
MYOP (OT, IT, IMT, qrshrun_n, IS, OS)

FUNC (int8x16_t, int16x8_t, int8x8_t, s16, s8)
FUNC (int16x8_t, int32x4_t, int16x4_t, s32, s16)
FUNC (int32x4_t, int64x2_t, int32x2_t, s64, s32)
FUNCUN (uint8x16_t, int16x8_t, uint8x8_t, s16, u8)
FUNCUN (uint16x8_t, int32x4_t, uint16x4_t, s32, u16)
FUNCUN (uint32x4_t, int64x2_t, uint32x2_t, s64, u32)

FUNC (uint8x16_t, uint16x8_t, uint8x8_t, u16, u8)
FUNC (uint16x8_t, uint32x4_t, uint16x4_t, u32, u16)
FUNC (uint32x4_t, uint64x2_t, uint32x2_t, u64, u32)


/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

