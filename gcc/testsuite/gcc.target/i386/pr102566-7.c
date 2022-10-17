/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <stdatomic.h>
#include <stdbool.h>

typedef int __attribute__ ((mode (__word__))) int_type;

#define BIT (1 << 0)

bool
foo0 (_Atomic int_type *v)
{
  return atomic_fetch_or_explicit (v, BIT, memory_order_relaxed) & ~1;
}

bool
foo1 (_Atomic int_type *v)
{
  return atomic_fetch_or_explicit (v, BIT, memory_order_relaxed) & ~2;
}

bool
foo2 (_Atomic int_type *v)
{
  return atomic_fetch_or_explicit (v, BIT, memory_order_relaxed) & ~3;
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*cmpxchg" 3 } } */
/* { dg-final { scan-assembler-not "bts" } } */
