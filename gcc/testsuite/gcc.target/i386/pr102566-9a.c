/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdatomic.h>
#include <stdbool.h>

bool
foo0 (_Atomic int *v)
{
#define BIT (1 << 0)
  return !(atomic_fetch_and_explicit (v, ~BIT, memory_order_relaxed) & BIT);
#undef BIT
}

bool
foo30 (_Atomic int *v)
{
#define BIT (1 << 30)
  return !(atomic_fetch_and_explicit (v, ~BIT, memory_order_relaxed) & BIT);
#undef BIT
}

bool
foo31 (_Atomic int *v)
{
#define BIT (1 << 31)
  return !(atomic_fetch_and_explicit (v, ~BIT, memory_order_relaxed) & BIT);
#undef BIT
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btrl" 3 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
