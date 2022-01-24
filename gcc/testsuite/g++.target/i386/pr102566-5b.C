/* { dg-do compile { target { c++11 && { ! ia32 } } } } */
/* { dg-options "-O2" } */

#include <atomic>

bool
tbit0 (std::atomic<unsigned long long> &i)
{
#define BIT (1ll << 0)
  return i.fetch_and(~BIT, std::memory_order_relaxed) & BIT;
#undef BIT 
}

bool
tbit30 (std::atomic<unsigned long long> &i)
{
#define BIT (1ll << 30)
  return i.fetch_and(~BIT, std::memory_order_relaxed) & BIT;
#undef BIT 
}

bool
tbit31 (std::atomic<unsigned long long> &i)
{
#define BIT (1ll << 63)
  return i.fetch_and(~BIT, std::memory_order_relaxed) & BIT;
#undef BIT 
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btrq" 3 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
