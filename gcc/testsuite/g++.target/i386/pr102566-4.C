/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2" } */

#include <atomic>

typedef int __attribute__ ((mode (__word__))) int_type;

#define BIT (1 << 0)

bool
tbit0 (std::atomic<int_type> &i)
{
  return i.fetch_or(BIT, std::memory_order_relaxed) & ~1;
}

bool
tbit30 (std::atomic<int_type> &i)
{
  return i.fetch_or(BIT, std::memory_order_relaxed) & ~2;
}

bool
tbit31 (std::atomic<int_type> &i)
{
  return i.fetch_or(BIT, std::memory_order_relaxed) & ~4;
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*cmpxchg" 3 } } */
/* { dg-final { scan-assembler-not "bts" } } */
