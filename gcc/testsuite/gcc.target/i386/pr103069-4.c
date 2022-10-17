/* PR target/103069 */
/* { dg-do run } */
/* { dg-additional-options "-O2 -march=x86-64 -mtune=generic" } */ 

#include <stdlib.h>
#include "pr103069-3.c"

#define FUNC_CMPXCHG_RELAX(TYPE) \
__attribute__ ((noinline, noclone, target ("relax-cmpxchg-loop")))	\
TYPE relax_##TYPE##_cmpxchg (TYPE *lock, TYPE newval, TYPE oldval)  \
{ \
  do  \
  { \
    newval = oldval | 1;  \
  } while (! __atomic_compare_exchange_n (lock, &oldval, newval,  \
					  0, __ATOMIC_RELEASE,  \
					  __ATOMIC_RELAXED));  \
  return *lock;	\
}

FUNC_CMPXCHG_RELAX (int64_t)
FUNC_CMPXCHG_RELAX (int)
FUNC_CMPXCHG_RELAX (short)
FUNC_CMPXCHG_RELAX (char)

#define TEST_CMPXCHG_LOOP(TYPE)	\
{ \
  TYPE a = 11, b = 20, c = 11, res, exp; \
  TYPE d = 11, e = 20, f = 11;	\
  res = relax_##TYPE##_cmpxchg (&a, b, c); \
  exp = f_##TYPE##_cmpxchg (&d, e, f); \
  if (res != exp || a != d) \
    abort (); \
}

int main (void)
{
  TEST_CMPXCHG_LOOP (int64_t)
  TEST_CMPXCHG_LOOP (int)
  TEST_CMPXCHG_LOOP (short)
  TEST_CMPXCHG_LOOP (char)
  return 0;
}
