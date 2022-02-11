/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdatomic.h>
#include <stdbool.h>

#define MASK 0x1234

bool
foo1 (_Atomic long *v)
{
  return atomic_fetch_and_explicit (v, ~MASK, memory_order_relaxed) & MASK;
}

bool
foo2 (_Atomic long *v, long mask)
{
  return atomic_fetch_and_explicit (v, ~mask, memory_order_relaxed) & mask;
}

bool
foo3 (_Atomic long *v, long mask)
{
  return !(atomic_fetch_and_explicit (v, ~mask, memory_order_relaxed) & mask);
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*cmpxchg" 3 } } */
/* { dg-final { scan-assembler-not "btr" } } */
