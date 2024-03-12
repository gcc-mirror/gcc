/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O -march=armv8.3-a+fp16" } */

#include <arm_neon.h>

#define BINARY(OT,IT,OP,S)                         \
OT                                              \
foo_##OP##_##S (IT a, IT b, IT c)                 \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b), zeros);      \
}

#define FUNC(T,IS,OS,OP,S) BINARY (T##x##OS##_t, T##x##IS##_t, OP, S)

#define OPTWO(T,IS,OS,S,OP1,OP2)        \
FUNC (T, IS, OS, OP1, S)                \
FUNC (T, IS, OS, OP2, S)

#define OPTHREE(T, IS, OS, S, OP1, OP2, OP3)    \
FUNC (T, IS, OS, OP1, S)        \
OPTWO (T, IS, OS, S, OP2, OP3)

#define OPFOUR(T,IS,OS,S,OP1,OP2,OP3,OP4)       \
FUNC (T, IS, OS, OP1, S)                \
OPTHREE (T, IS, OS, S, OP2, OP3, OP4)

OPTWO (float16, 4, 8, f16, cadd_rot90, cadd_rot270)
OPTWO (float32, 2, 4, f32, cadd_rot90, cadd_rot270)

#define TERNARY(OT,IT,OP,S)                         \
OT                                              \
foo_##OP##_##S (IT a, IT b, IT c)                 \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b, c), zeros);      \
}

#undef FUNC
#define FUNC(T,IS,OS,OP,S) TERNARY (T##x##OS##_t, T##x##IS##_t, OP, S)

OPFOUR (float16, 4, 8, f16, cmla, cmla_rot90, cmla_rot180, cmla_rot270)
OPFOUR (float32, 2, 4, f32, cmla, cmla_rot90, cmla_rot180, cmla_rot270)

#define TERNARY_IDX(OT,IT,OP,S)                         \
OT                                              \
foo_##OP##_##S (IT a, IT b, IT c)                 \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b, c, 0), zeros);      \
}

#undef FUNC
#define FUNC(T,IS,OS,OP,S) TERNARY_IDX (T##x##OS##_t, T##x##IS##_t, OP, S)
OPFOUR (float16, 4, 8, f16, cmla_lane, cmla_rot90_lane, cmla_rot180_lane, cmla_rot270_lane)
OPFOUR (float32, 2, 4, f32, cmla_lane, cmla_rot90_lane, cmla_rot180_lane, cmla_rot270_lane)

/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

