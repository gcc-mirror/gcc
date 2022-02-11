/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

#include <stdatomic.h>
#include <stdbool.h>

bool
foo (_Atomic long long int *v, int bit)
{
  long long int mask = 1ll << bit;
  return atomic_fetch_or_explicit (v, mask, memory_order_relaxed) & mask;
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btsq" 1 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
