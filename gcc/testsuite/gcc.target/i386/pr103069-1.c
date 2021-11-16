/* PR target/103068 */
/* { dg-do compile } */
/* { dg-additional-options "-O2 -march=x86-64 -mtune=generic -mrelax-cmpxchg-loop" } */ 
/* { dg-final { scan-assembler-times "rep;?\[ \\t\]+nop" 32 } } */

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

FUNC_ATOMIC (int64_t, and)
FUNC_ATOMIC (int64_t, nand)
FUNC_ATOMIC (int64_t, or)
FUNC_ATOMIC (int64_t, xor)
FUNC_ATOMIC (int, and)
FUNC_ATOMIC (int, nand)
FUNC_ATOMIC (int, or)
FUNC_ATOMIC (int, xor)
FUNC_ATOMIC (short, and)
FUNC_ATOMIC (short, nand)
FUNC_ATOMIC (short, or)
FUNC_ATOMIC (short, xor)
FUNC_ATOMIC (char, and)
FUNC_ATOMIC (char, nand)
FUNC_ATOMIC (char, or)
FUNC_ATOMIC (char, xor)
