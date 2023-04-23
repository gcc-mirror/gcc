/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

#define ONE(OT,IT,OP,S)                         \
OT                                              \
foo_##OP##_##S (IT a, IT b)                     \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S (v##OP##_##S (a, b), zeros);      \
}

#define FUNC(T,IS,OS,OP,S) ONE (T##x##OS##_t, T##x##IS##_t, OP, S)

#define OPTWO(T,IS,OS,S,OP1,OP2)        \
FUNC (T, IS, OS, OP1, S)                \
FUNC (T, IS, OS, OP2, S)

#define OPTHREE(T, IS, OS, S, OP1, OP2, OP3)    \
FUNC (T, IS, OS, OP1, S)        \
OPTWO (T, IS, OS, S, OP2, OP3)

#define OPFOUR(T,IS,OS,S,OP1,OP2,OP3,OP4)       \
FUNC (T, IS, OS, OP1, S)                \
OPTHREE (T, IS, OS, S, OP2, OP3, OP4)

#define OPFIVE(T,IS,OS,S,OP1,OP2,OP3,OP4, OP5)  \
FUNC (T, IS, OS, OP1, S)                \
OPFOUR (T, IS, OS, S, OP2, OP3, OP4, OP5)

#define OPSIX(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6)        \
FUNC (T, IS, OS, OP1, S)                \
OPFIVE (T, IS, OS, S, OP2, OP3, OP4, OP5, OP6)

#define OPSEVEN(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6,OP7)        \
FUNC (T, IS, OS, OP1, S)                \
OPSIX (T, IS, OS, S, OP2, OP3, OP4, OP5, OP6, OP7)


OPSEVEN (int8, 8, 16, s8, padd, add, sub, mul, and, orr, eor)
OPSEVEN (int16, 4, 8, s16, padd, add, sub, mul, and, orr, eor)
OPSEVEN (int32, 2, 4, s32, padd, add, sub, mul, and, orr, eor)

OPSEVEN (uint8, 8, 16, u8, padd, add, sub, mul, and, orr, eor)
OPSEVEN (uint16, 4, 8, u16, padd, add, sub, mul, and, orr, eor)
OPSEVEN (uint32, 2, 4, u32, padd, add, sub, mul, and, orr, eor)

/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

