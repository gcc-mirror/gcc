/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

#define MYOP(OT,IT1,IT2,OP,OS)                         \
OT                                              \
foo_##OP##_##OS (IT1 a, IT2 b)                     \
{                                               \
  IT1 zeros = vcreate_##OS (0);                   \
  return vcombine_##OS (v##OP##_##OS (a, b), zeros);      \
}

MYOP (int8x16_t, int8x8_t, uint8x8_t, uqadd, s8)
MYOP (int16x8_t, int16x4_t, uint16x4_t, uqadd, s16)
MYOP (int32x4_t, int32x2_t, uint32x2_t, uqadd, s32)
MYOP (int64x2_t, int64x1_t, uint64x1_t, uqadd, s64)

MYOP (uint8x16_t, uint8x8_t, int8x8_t, sqadd, u8)
MYOP (uint16x8_t, uint16x4_t, int16x4_t, sqadd, u16)
MYOP (uint32x4_t, uint32x2_t, int32x2_t, sqadd, u32)
MYOP (uint64x2_t, uint64x1_t, int64x1_t, sqadd, u64)

MYOP (uint8x16_t, uint8x8_t, int8x8_t, shl, u8)
MYOP (uint16x8_t, uint16x4_t, int16x4_t, shl, u16)
MYOP (uint32x4_t, uint32x2_t, int32x2_t, shl, u32)
MYOP (uint64x2_t, uint64x1_t, int64x1_t, shl, u64)

MYOP (uint8x16_t, uint8x8_t, int8x8_t, qshl, u8)
MYOP (uint16x8_t, uint16x4_t, int16x4_t, qshl, u16)
MYOP (uint32x4_t, uint32x2_t, int32x2_t, qshl, u32)
MYOP (uint64x2_t, uint64x1_t, int64x1_t, qshl, u64)

/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

