/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

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

#define OPFOURTEEN(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6,OP7,OP8,OP9,OP10,OP11,OP12,OP13,OP14)        \
OPSEVEN (T, IS, OS, S, OP1, OP2, OP3, OP4, OP5, OP6, OP7)                \
OPSEVEN (T, IS, OS, S, OP8, OP9, OP10, OP11, OP12, OP13, OP14)

#define OPNINETEEN(T,IS,OS,S,OP1,OP2,OP3,OP4,OP5,OP6,OP7,OP8,OP9,OP10,OP11,OP12,OP13,OP14,OP15,OP16,OP17,OP18,OP19)        \
OPEIGHT (T, IS, OS, S, OP1, OP2, OP3, OP4, OP5, OP6, OP7, OP8)                \
OPELEVEN (T, IS, OS, S, OP9, OP10, OP11, OP12, OP13, OP14, OP15, OP16, OP17, OP18, OP19)

OPNINETEEN (int8, 8, 16, s8, padd, add, qadd, qsub, sub, mul, and, orr, eor, orn, bic, max, min, hadd, rhadd, hsub, abd, pmax, pmin)
OPNINETEEN (int16, 4, 8, s16, padd, add, qadd, qsub, sub, mul, and, orr, eor, orn, bic, max, min, hadd, rhadd, hsub, abd, pmax, pmin)
OPNINETEEN (int32, 2, 4, s32, padd, add, qadd, qsub, sub, mul, and, orr, eor, orn, bic, max, min, hadd, rhadd, hsub, abd, pmax, pmin)

OPSIX (int8, 8, 16, s8, zip1, zip2, uzp1, uzp2, shl, qshl)
OPEIGHT (int16, 4, 8, s16, zip1, zip2, uzp1, uzp2, shl, qshl, qdmulh, qrdmulh)
OPEIGHT (int32, 2, 4, s32, zip1, zip2, uzp1, uzp2, shl, qshl, qdmulh, qrdmulh)

OPNINETEEN (uint8, 8, 16, u8, padd, add, qadd, qsub, sub, mul, and, orr, eor, orn, bic, max, min, hadd, rhadd, hsub, abd, pmax, pmin)
OPNINETEEN (uint16, 4, 8, u16, padd, add, qadd, qsub, sub, mul, and, orr, eor, orn, bic, max, min, hadd, rhadd, hsub, abd, pmax, pmin)
OPNINETEEN (uint32, 2, 4, u32, padd, add, qadd, qsub, sub, mul, and, orr, eor, orn, bic, max, min, hadd, rhadd, hsub, abd, pmax, pmin)

OPFOUR (uint8, 8, 16, u8, zip1, zip2, uzp1, uzp2)
OPFOUR (uint16, 4, 8, u16, zip1, zip2, uzp1, uzp2)
OPFOUR (uint32, 2, 4, u32, zip1, zip2, uzp1, uzp2)

OPFOURTEEN (float32, 2, 4, f32, add, padd, sub, mul, div, max, maxnm, min, minnm, abd, pmax, pmin, pmaxnm, pminnm)

#define UNARY(OT,IT,OP,S)			\
OT                                              \
foo_##IT##OP##_##S (IT a)                     \
{                                               \
  IT zeros = vcreate_##S (0);                   \
  return vcombine_##S ((IT) v##OP##_##S (a), zeros);      \
}

#undef FUNC
#define FUNC(T,IS,OS,OP,S) UNARY (T##x##OS##_t, T##x##IS##_t, OP, S)
OPTEN (int8, 8, 16, s8, neg, abs, rbit, clz, cls, cnt, mvn, rev16, rev32, rev64)
OPSEVEN (int16, 4, 8, s16, neg, abs, clz, cls, mvn, rev32, rev64)
OPSIX (int32, 2, 4, s32, neg, abs, clz, cls, mvn, rev64)

OPEIGHT (uint8, 8, 16, u8, rbit, clz, cnt, cls, mvn, rev16, rev32, rev64)
OPFIVE (uint16, 4, 8, u16, clz, cls, mvn, rev32, rev64)
OPFOUR (uint32, 2, 4, u32, clz, cls, mvn, rev64)

OPTEN (float32, 2, 4, f32, neg, abs, sqrt, rnd, rndi, rndm, rnda, rndn, rndp, rndx)
/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

