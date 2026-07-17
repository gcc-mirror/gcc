/* { dg-do run { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -march=x86-64 -m128bit-atomic" } */

#include <stdlib.h>
#include "pr126293-2a.c"

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

FUNC_ATOMIC_RELAX (__int128_t, and)
FUNC_ATOMIC_RELAX (__int128_t, nand)
FUNC_ATOMIC_RELAX (__int128_t, or)
FUNC_ATOMIC_RELAX (__int128_t, xor)

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

__attribute__((noinline))
static void
do_test (void)
{
  TEST_ATOMIC_FETCH_LOGIC (__int128_t, and)
  TEST_ATOMIC_FETCH_LOGIC (__int128_t, nand)
  TEST_ATOMIC_FETCH_LOGIC (__int128_t, or)
  TEST_ATOMIC_FETCH_LOGIC (__int128_t, xor)
}

int
main (void)
{
  if (__builtin_cpu_supports ("cmpxchg16b"))
    do_test ();
  return 0;
}
