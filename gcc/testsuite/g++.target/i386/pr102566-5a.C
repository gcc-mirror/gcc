/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2" } */

#include <atomic>

bool
tbit0 (std::atomic<unsigned int> &i)
{
#define BIT (1 << 0)
  return i.fetch_and(~BIT, std::memory_order_relaxed) & BIT;
#undef BIT 
}

bool
tbit30 (std::atomic<unsigned int> &i)
{
#define BIT (1 << 30)
  return i.fetch_and(~BIT, std::memory_order_relaxed) & BIT;
#undef BIT 
}

bool
tbit31 (std::atomic<unsigned int> &i)
{
#define BIT (1 << 31)
  return i.fetch_and(~BIT, std::memory_order_relaxed) & BIT;
#undef BIT 
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btrl" 3 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
