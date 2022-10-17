/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdatomic.h>
#include <stdbool.h>

bool
foo (_Atomic int *v, int bit)
{
  int mask = 1 << bit;
  return atomic_fetch_or_explicit (v, mask, memory_order_relaxed) & mask;
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btsl" 1 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
