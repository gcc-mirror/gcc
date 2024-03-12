/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

#define TERNARY(OT,IT,OP,S)                         \
OT                                              \
foo_##OP##_##S (IT a, IT b, IT c)                     \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b, c), zeros);      \
}

#define FUNC(T,IS,OS,OP,S) TERNARY (T##x##OS##_t, T##x##IS##_t, OP, S)

#define OPTWO(T,IS,OS,S,OP1,OP2)        \
FUNC (T, IS, OS, OP1, S)                \
FUNC (T, IS, OS, OP2, S)

#define OPTHREE(T, IS, OS, S, OP1, OP2, OP3)    \
FUNC (T, IS, OS, OP1, S)        \
OPTWO (T, IS, OS, S, OP2, OP3)

#define OPFOUR(T,IS,OS,S,OP1,OP2,OP3,OP4)       \
FUNC (T, IS, OS, OP1, S)                \
OPTHREE (T, IS, OS, S, OP2, OP3, OP4)

OPTHREE (int8, 8, 16, s8, mla, mls, aba)
OPTHREE (int16, 4, 8, s16, mla, mls, aba)
OPTHREE (int32, 2, 4, s32, mla, mls, aba)

OPFOUR (uint8, 8, 16, u8, mla, mls, aba, bsl)
OPFOUR (uint16, 4, 8, u16, mla, mls, aba, bsl)
OPFOUR (uint32, 2, 4, u32, mla, mls, aba, bsl)

OPTHREE (float32, 2, 4, f32, mla, fma, fms)

#undef FUNC
#define TERNARY_LANE(OT,IT,OP,S)                         \
OT                                              \
foo_##OP##_##S (IT a, IT b, IT c)                     \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b, c, 0), zeros);      \
}						\
OT                                              \
foo_##OP##_##S##_lane1 (IT a, IT b, IT c)                     \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b, c, 1), zeros);      \
}

#define FUNC(T,IS,OS,OP,S) TERNARY_LANE (T##x##OS##_t, T##x##IS##_t, OP, S)
OPTWO (int16, 4, 8, s16, mla_lane, mls_lane)
OPTWO (int32, 2, 4, s32, mla_lane, mls_lane)

OPTWO (uint16, 4, 8, u16, mla_lane, mls_lane)
OPTWO (uint32, 2, 4, u32, mla_lane, mls_lane)

OPTHREE (float32, 2, 4, f32, mla_lane, fma_lane, fms_lane)

/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

