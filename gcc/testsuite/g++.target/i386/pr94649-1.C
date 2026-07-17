/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -std=c++11 -march=x86-64 -m128bit-atomic" } */
/* { dg-final { scan-assembler-times "lock;?\[ \\t\]+cmpxchg16b" 1 } } */

#include <atomic>

struct alignas(16) a
{
  long x;
  long y;
};

bool
cmpxchg(std::atomic<a>& data, a expected, a newval)
{
  return std::atomic_compare_exchange_weak(&data, &expected, newval);
}
