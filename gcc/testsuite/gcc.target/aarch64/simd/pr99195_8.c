/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

#define MYOP(OT,IT1,OP,S,OS)					\
OT								\
foo_##OP##_##S##OS (IT1 a)					\
{								\
  IT1 zeros = vcreate_##S##OS (0);				\
  return vcombine_##S##OS (v##OP##_##S##OS (a, 3), zeros);	\
}								\
OT								\
foo_##OP##_##S##OS##_s (IT1 a)					\
{								\
  IT1 zeros = vcreate_##S##OS (0);				\
  return vcombine_##S##OS (v##OP##_##S##OS (a, OS - 1), zeros);	\
}

MYOP (int8x16_t, int8x8_t, shr_n, s, 8)
MYOP (int16x8_t, int16x4_t, shr_n, s, 16)
MYOP (int32x4_t, int32x2_t, shr_n, s, 32)
MYOP (uint8x16_t, uint8x8_t, shr_n, u, 8)
MYOP (uint16x8_t, uint16x4_t, shr_n, u, 16)
MYOP (uint32x4_t, uint32x2_t, shr_n, u, 32)
MYOP (int8x16_t, int8x8_t, shl_n, s, 8)
MYOP (int16x8_t, int16x4_t, shl_n, s, 16)
MYOP (int32x4_t, int32x2_t, shl_n, s, 32)
MYOP (uint8x16_t, uint8x8_t, shl_n, u, 8)
MYOP (uint16x8_t, uint16x4_t, shl_n, u, 16)
MYOP (uint32x4_t, uint32x2_t, shl_n, u, 32)

/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

