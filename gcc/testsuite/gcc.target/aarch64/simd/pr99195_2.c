/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O -march=armv8.2-a+fp16" } */

#include <arm_neon.h>

#define BINARY(OT,IT,OP,S)                         \
OT                                              \
foo_##OP##_##S (IT a, IT b)                     \
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

#define OPFIVE(T,IS,OS,S,OP1,OP2,OP3,OP4, OP5)  \
FUNC (T, IS, OS, OP1, S)                \
OPFOUR (T, IS, OS, S, OP2, OP3, OP4, OP5)

#define OPSIX(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6)        \
FUNC (T, IS, OS, OP1, S)                \
OPFIVE (T, IS, OS, S, OP2, OP3, OP4, OP5, OP6)

#define OPSEVEN(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6,OP7)        \
FUNC (T, IS, OS, OP1, S)                \
OPSIX (T, IS, OS, S, OP2, OP3, OP4, OP5, OP6, OP7)

#define OPEIGHT(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6,OP7,OP8)        \
OPTHREE (T, IS, OS, S, OP1, OP2, OP3)                \
OPFIVE (T, IS, OS, S, OP4, OP5, OP6, OP7, OP8)

#define OPTEN(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6,OP7,OP8,OP9,OP10)        \
OPFIVE (T, IS, OS, S, OP1, OP2, OP3, OP4, OP5)                \
OPFIVE (T, IS, OS, S, OP6, OP7, OP8, OP9, OP10)

#define OPELEVEN(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6,OP7,OP8,OP9,OP10,OP11)        \
OPFIVE (T, IS, OS, S, OP1, OP2, OP3, OP4, OP5)                \
OPSIX (T, IS, OS, S, OP6, OP7, OP8, OP9, OP10, OP11)

#define OPTHIRTEEN(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6,OP7,OP8,OP9,OP10,OP11,OP12,OP13)        \
OPSIX (T, IS, OS, S, OP1, OP2, OP3, OP4, OP5, OP6)                \
OPSEVEN (T, IS, OS, S, OP7, OP8, OP9, OP10, OP11, OP12, OP13)

OPTHIRTEEN (float16, 4, 8, f16, add, sub, mul, div, max, maxnm, min, minnm, abd, pmax, pmin, pmaxnm, pminnm)

#define UNARY(OT,IT,OP,S)			\
OT                                              \
foo_##IT##OP##_##S (IT a)                     \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S ((IT) v##OP##_##S (a), zeros);      \
}

#undef FUNC
#define FUNC(T,IS,OS,OP,S) UNARY (T##x##OS##_t, T##x##IS##_t, OP, S)

OPTEN (float16, 4, 8, f16, neg, abs, sqrt, rnd, rndi, rndm, rnda, rndn, rndp, rndx)
/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

