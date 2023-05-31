/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O -march=armv8.2-a+dotprod" } */

#include <arm_neon.h>

#define DOTPROD(OT,AT,IT1,IT2,OP,S)                         \
OT                                              \
foo_##OP##_##S (AT a, IT1 b, IT2 c)                 \
{                                               \
  AT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b, c), zeros);      \
}

#define DOTPROD_IDX(OT,AT,IT1,IT2,OP,S)                         \
OT                                              \
foo_##OP##_##S (AT a, IT1 b, IT2 c)                 \
{                                               \
  AT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b, c, 1), zeros);      \
}

DOTPROD (int32x4_t, int32x2_t, int8x8_t, int8x8_t, dot, s32)
DOTPROD (uint32x4_t, uint32x2_t, uint8x8_t, uint8x8_t, dot, u32)
DOTPROD_IDX (int32x4_t, int32x2_t, int8x8_t, int8x8_t, dot_lane, s32)
DOTPROD_IDX (uint32x4_t, uint32x2_t, uint8x8_t, uint8x8_t, dot_lane, u32)

#pragma GCC target ("+i8mm")
DOTPROD (int32x4_t, int32x2_t, uint8x8_t, int8x8_t, usdot, s32)
DOTPROD_IDX (int32x4_t, int32x2_t, uint8x8_t, int8x8_t, usdot_lane, s32)
DOTPROD_IDX (int32x4_t, int32x2_t, int8x8_t, uint8x8_t, sudot_lane, s32)

/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

