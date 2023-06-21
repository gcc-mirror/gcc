/* PR target/99195.  */
/*  Check that we take advantage of 64-bit Advanced SIMD operations clearing
    the top half of the vector register and no explicit zeroing instructions
    are emitted.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

#include <arm_neon.h>

#define MYOP(OT,IT,IMT,OP,IS,OS)			\
OT							\
foo_##OP##_##OS##_##IT##_##IS (IT a, IT b)		\
{							\
  IMT zeros = vcreate_##OS (0);				\
  return vcombine_##OS (v##OP##_##IS (a, b), zeros);	\
}

#define FUNC(OT,IT,IMT,IS,OS)		\
MYOP (OT, IT, IMT, ceq, IS, OS)		\
MYOP (OT, IT, IMT, clt, IS, OS)		\
MYOP (OT, IT, IMT, cge, IS, OS)		\
MYOP (OT, IT, IMT, cle, IS, OS)		\
MYOP (OT, IT, IMT, cgt, IS, OS)		\
MYOP (OT, IT, IMT, tst, IS, OS)

#define MYFUNC(PFX, T, S, N, DN)		\
FUNC (uint##S##x##DN##_t, T##S##x##N##_t, uint##S##x##N##_t, PFX##S, u##S)

MYFUNC (s, int, 8, 8, 16)
MYFUNC (s, int, 16, 4, 8)
MYFUNC (s, int, 32, 2, 4)
MYFUNC (u, uint, 8, 8, 16)
MYFUNC (u, uint, 16, 4, 8)
MYFUNC (u, uint, 32, 2, 4)

#undef FUNC
#define FUNC(OT,IT,IMT,IS,OS)		\
MYOP (OT, IT, IMT, ceq, IS, OS)		\
MYOP (OT, IT, IMT, clt, IS, OS)		\
MYOP (OT, IT, IMT, cge, IS, OS)		\
MYOP (OT, IT, IMT, cle, IS, OS)		\
MYOP (OT, IT, IMT, cgt, IS, OS)


#pragma GCC push_options
#pragma GCC target ("arch=armv8.2-a+fp16")
MYFUNC (f, float, 16, 4, 8)
#pragma GCC pop_options
MYFUNC (f, float, 32, 2, 4)
MYFUNC (f, float, 64, 1, 2)

#undef FUNC
#define FUNC(OT,IT,IMT,IS,OS)			\
MYOP (OT, IT, IMT, cale, IS, OS)		\
MYOP (OT, IT, IMT, cagt, IS, OS)		\
MYOP (OT, IT, IMT, calt, IS, OS)		\
MYOP (OT, IT, IMT, cage, IS, OS)		\

#pragma GCC push_options
#pragma GCC target ("arch=armv8.2-a+fp16")
MYFUNC (f, float, 16, 4, 8)
#pragma GCC pop_options
MYFUNC (f, float, 32, 2, 4)
MYFUNC (f, float, 64, 1, 2)

#undef MYOP
#define MYOP(OT,IT,IMT,OP,IS,OS)			\
OT							\
foo_##OP##_##OS##_##IT##_z (IT a)			\
{							\
  IMT zeros = vcreate_##OS (0);				\
  return vcombine_##OS (v##OP##_##IS (a), zeros);	\
}

#undef FUNC
#define FUNC(OT,IT,IMT,IS,OS)			\
MYOP (OT, IT, IMT, cltz, IS, OS)		\
MYOP (OT, IT, IMT, ceqz, IS, OS)		\
MYOP (OT, IT, IMT, cgez, IS, OS)		\
MYOP (OT, IT, IMT, cgtz, IS, OS)		\
MYOP (OT, IT, IMT, clez, IS, OS)		\

MYFUNC (s, int, 8, 8, 16)
MYFUNC (s, int, 16, 4, 8)
MYFUNC (s, int, 32, 2, 4)

#pragma GCC push_options
#pragma GCC target ("arch=armv8.2-a+fp16")
MYFUNC (f, float, 16, 4, 8)
#pragma GCC pop_options
MYFUNC (f, float, 32, 2, 4)
MYFUNC (f, float, 64, 1, 2)

/* { dg-final { scan-assembler-not {\tfmov\t} } }  */
/* { dg-final { scan-assembler-not {\tmov\t} } }  */

