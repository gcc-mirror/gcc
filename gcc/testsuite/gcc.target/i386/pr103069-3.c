/* PR target/103068 */
/* { dg-do compile } */
/* { dg-additional-options "-O2 -march=x86-64 -mtune=generic -mrelax-cmpxchg-loop" } */ 

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


FUNC_CMPXCHG (int64_t)
FUNC_CMPXCHG (int)
FUNC_CMPXCHG (short)
FUNC_CMPXCHG (char)
