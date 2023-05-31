/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O -march=armv8.1-a+rdma" } */

#include <arm_neon.h>

#define OPTWO(T,IS,OS,S,OP1,OP2)        \
FUNC (T, IS, OS, OP1, S)                \
FUNC (T, IS, OS, OP2, S)

#define TERNARY(OT,IT,OP,S)                         \
OT                                              \
foo_##OP##_##S (IT a, IT b, IT c)                 \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b, c), zeros);      \
}

#undef FUNC
#define FUNC(T,IS,OS,OP,S) TERNARY (T##x##OS##_t, T##x##IS##_t, OP, S)

OPTWO (int16, 4, 8, s16, qrdmlah, qrdmlsh)
OPTWO (int32, 2, 4, s32, qrdmlah, qrdmlsh)

#define TERNARY_IDX(OT,IT,OP,S)                         \
OT                                              \
foo_##OP##_##S (IT a, IT b, IT c)                 \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b, c, 0), zeros);      \
}

#undef FUNC
#define FUNC(T,IS,OS,OP,S) TERNARY_IDX (T##x##OS##_t, T##x##IS##_t, OP, S)
OPTWO (int16, 4, 8, s16, qrdmlah_lane, qrdmlsh_lane)
OPTWO (int32, 2, 4, s32, qrdmlah_lane, qrdmlsh_lane)

/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

