/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -march=x86-64 -m128bit-atomic -mrelax-cmpxchg-loop" } */
/* { dg-final { scan-assembler-times "lock;?\[ \\t\]+cmpxchg16b" 1 } } */

#include <stdint.h>

#define FUNC_CMPXCHG(TYPE) \
__attribute__ ((noinline, noclone))	\
TYPE f_##TYPE##_cmpxchg (TYPE *lock, TYPE newval, TYPE oldval)  \
{ \
  do  \
  { \
    newval = oldval | 1;  \
  } while (! __atomic_compare_exchange_n (lock, &oldval, newval,  \
					  0, __ATOMIC_RELEASE,  \
					  __ATOMIC_RELAXED));  \
  return *lock;	\
}


FUNC_CMPXCHG (__int128_t)
