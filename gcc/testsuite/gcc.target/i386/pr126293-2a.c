/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -march=x86-64 -m128bit-atomic -mrelax-cmpxchg-loop" } */
/* { dg-final { scan-assembler-times "rep;?\[ \\t\]+nop" 8 } } */
/* { dg-final { scan-assembler-times "lock;?\[ \\t\]+cmpxchg16b" 8 } } */

#include <stdint.h>

#define FUNC_ATOMIC(TYPE, OP) \
__attribute__ ((noinline, noclone))	\
TYPE f_##TYPE##_##OP##_fetch (TYPE *a, TYPE b)	\
{ \
  return __atomic_##OP##_fetch (a, b, __ATOMIC_RELAXED);  \
} \
__attribute__ ((noinline, noclone))	\
TYPE f_##TYPE##_fetch_##OP (TYPE *a, TYPE b)	\
{ \
  return __atomic_fetch_##OP (a, b, __ATOMIC_RELAXED);  \
}

FUNC_ATOMIC (__int128_t, and)
FUNC_ATOMIC (__int128_t, nand)
FUNC_ATOMIC (__int128_t, or)
FUNC_ATOMIC (__int128_t, xor)
