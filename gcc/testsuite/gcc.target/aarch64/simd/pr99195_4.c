/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

#define MYOP(OT,IT,IMT,OP,IS,OS)                         \
OT                                              \
foo_##OP##_##OS (IT a, IT b)                     \
{                                               \
  IMT zeros = vcreate_##OS (0);                   \
  return vcombine_##OS (v##OP##_##IS (a, b), zeros);      \
}

#define FUNC(OT,IT,IMT,IS,OS)			\
MYOP (OT, IT, IMT, addhn, IS, OS)		\
MYOP (OT, IT, IMT, subhn, IS, OS)		\
MYOP (OT, IT, IMT, raddhn, IS, OS)		\
MYOP (OT, IT, IMT, rsubhn, IS, OS)

FUNC (int8x16_t, int16x8_t, int8x8_t, s16, s8)
FUNC (int16x8_t, int32x4_t, int16x4_t, s32, s16)
FUNC (int32x4_t, int64x2_t, int32x2_t, s64, s32)

FUNC (uint8x16_t, uint16x8_t, uint8x8_t, u16, u8)
FUNC (uint16x8_t, uint32x4_t, uint16x4_t, u32, u16)
FUNC (uint32x4_t, uint64x2_t, uint32x2_t, u64, u32)

#undef MYOP
#define MYOP(OT,IT,IMT,OP,IS,OS)		\
OT						\
foo_##OP##_##OS (IT a)				\
{						\
  IMT zeros = vcreate_##OS (0);			\
  return vcombine_##OS (v##OP##_##IS (a), zeros);	\
}

#undef FUNC
#define FUNC(OP)					\
MYOP (int8x16_t, int16x8_t, int8x8_t, OP, s16, s8)	\
MYOP (int16x8_t, int32x4_t, int16x4_t, OP, s32, s16)	\
MYOP (int32x4_t, int64x2_t, int32x2_t, OP, s64, s32)	\
MYOP (uint8x16_t, uint16x8_t, uint8x8_t, OP, u16, u8)	\
MYOP (uint16x8_t, uint32x4_t, uint16x4_t, OP, u32, u16)	\
MYOP (uint32x4_t, uint64x2_t, uint32x2_t, OP, u64, u32)	\

FUNC (movn)
FUNC (qmovn)

#undef FUNC
#define FUNC(OP)					\
MYOP (int8x16_t, int8x8_t, int8x8_t, OP, s8, s8)	\
MYOP (int16x8_t, int16x4_t, int16x4_t, OP, s16, s16)	\
MYOP (int32x4_t, int32x2_t, int32x2_t, OP, s32, s32)	\
MYOP (int64x2_t, int64x1_t, int64x1_t, OP, s64, s64)	\

FUNC (qabs)
FUNC (qneg)

#undef FUNC
#define FUNC(OP)					\
MYOP (uint8x16_t, int16x8_t, uint8x8_t, OP, s16, u8)	\
MYOP (uint16x8_t, int32x4_t, uint16x4_t, OP, s32, u16)	\
MYOP (uint32x4_t, int64x2_t, uint32x2_t, OP, s64, u32)	\

FUNC (qmovun)

/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

