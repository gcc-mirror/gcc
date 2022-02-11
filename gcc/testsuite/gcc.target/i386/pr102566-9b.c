/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

#include <stdatomic.h>
#include <stdbool.h>

bool
foo0 (_Atomic long long *v)
{
#define BIT (1ll << 0)
  return !(atomic_fetch_and_explicit (v, ~BIT, memory_order_relaxed) & BIT);
#undef BIT
}

bool
foo30 (_Atomic long long *v)
{
#define BIT (1ll << 62)
  return !(atomic_fetch_and_explicit (v, ~BIT, memory_order_relaxed) & BIT);
#undef BIT
}

bool
foo31 (_Atomic long long *v)
{
#define BIT (1ll << 63)
  return !(atomic_fetch_and_explicit (v, ~BIT, memory_order_relaxed) & BIT);
#undef BIT
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btrq" 3 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
