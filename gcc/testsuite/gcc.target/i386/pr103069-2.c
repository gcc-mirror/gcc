/* PR target/103069 */
/* { dg-do run } */
/* { dg-additional-options "-O2 -march=x86-64 -mtune=generic" } */ 

#include <stdlib.h>
#include "pr103069-1.c"

#define FUNC_ATOMIC_RELAX(TYPE, OP) \
__attribute__ ((noinline, noclone, target ("relax-cmpxchg-loop")))	\
TYPE relax_##TYPE##_##OP##_fetch (TYPE *a, TYPE b)	\
{ \
  return __atomic_##OP##_fetch (a, b, __ATOMIC_RELAXED);  \
} \
__attribute__ ((noinline, noclone, target ("relax-cmpxchg-loop")))	\
TYPE relax_##TYPE##_fetch_##OP (TYPE *a, TYPE b)	\
{ \
  return __atomic_fetch_##OP (a, b, __ATOMIC_RELAXED);  \
}

FUNC_ATOMIC_RELAX (int64_t, and)
FUNC_ATOMIC_RELAX (int64_t, nand)
FUNC_ATOMIC_RELAX (int64_t, or)
FUNC_ATOMIC_RELAX (int64_t, xor)
FUNC_ATOMIC_RELAX (int, and)
FUNC_ATOMIC_RELAX (int, nand)
FUNC_ATOMIC_RELAX (int, or)
FUNC_ATOMIC_RELAX (int, xor)
FUNC_ATOMIC_RELAX (short, and)
FUNC_ATOMIC_RELAX (short, nand)
FUNC_ATOMIC_RELAX (short, or)
FUNC_ATOMIC_RELAX (short, xor)
FUNC_ATOMIC_RELAX (char, and)
FUNC_ATOMIC_RELAX (char, nand)
FUNC_ATOMIC_RELAX (char, or)
FUNC_ATOMIC_RELAX (char, xor)

#define TEST_ATOMIC_FETCH_LOGIC(TYPE, OP) \
{ \
  TYPE a = 11, b = 101, res, exp; \
  TYPE c = 11, d = 101;	\
  res = relax_##TYPE##_##OP##_fetch (&a, b); \
  exp = f_##TYPE##_##OP##_fetch (&c, d);  \
  if (res != exp || a != c) \
    abort (); \
  a = c = 21, b = d = 92; \
  res = relax_##TYPE##_fetch_##OP (&a, b); \
  exp = f_##TYPE##_fetch_##OP (&c, d);  \
  if (res != exp || a != c) \
    abort (); \
}

int main (void)
{
  TEST_ATOMIC_FETCH_LOGIC (int64_t, and)
  TEST_ATOMIC_FETCH_LOGIC (int64_t, nand)
  TEST_ATOMIC_FETCH_LOGIC (int64_t, or)
  TEST_ATOMIC_FETCH_LOGIC (int64_t, xor)
  TEST_ATOMIC_FETCH_LOGIC (int, and)
  TEST_ATOMIC_FETCH_LOGIC (int, nand)
  TEST_ATOMIC_FETCH_LOGIC (int, or)
  TEST_ATOMIC_FETCH_LOGIC (int, xor)
  TEST_ATOMIC_FETCH_LOGIC (short, and)
  TEST_ATOMIC_FETCH_LOGIC (short, nand)
  TEST_ATOMIC_FETCH_LOGIC (short, or)
  TEST_ATOMIC_FETCH_LOGIC (short, xor)
  TEST_ATOMIC_FETCH_LOGIC (char, and)
  TEST_ATOMIC_FETCH_LOGIC (char, nand)
  TEST_ATOMIC_FETCH_LOGIC (char, or)
  TEST_ATOMIC_FETCH_LOGIC (char, xor)
  return 0;
}
