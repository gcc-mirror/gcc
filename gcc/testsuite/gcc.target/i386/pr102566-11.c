/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdatomic.h>
#include <stdbool.h>

#define MASK 0x1234

bool
foo1 (_Atomic int *v)
{
  return atomic_fetch_or_explicit (v, MASK, memory_order_relaxed) & MASK;
}

bool
foo2 (_Atomic unsigned int *v, int mask)
{
  return atomic_fetch_or_explicit (v, mask, memory_order_relaxed) & mask;
}

bool
foo3 (_Atomic unsigned int *v, int mask)
{
  return !(atomic_fetch_or_explicit (v, mask, memory_order_relaxed) & mask);
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*cmpxchg" 3 } } */
/* { dg-final { scan-assembler-not "bts" } } */
