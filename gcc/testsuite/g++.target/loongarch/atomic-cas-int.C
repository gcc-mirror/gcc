/* { dg-do run } */
/* { dg-options "-O2" } */

#include <atomic>
#include <cstdio>

__attribute__ ((noinline)) long
val_without_const_folding (long val)
{
  return val;
}

int
main ()
{
  int oldval = 0xaa;
  int newval = 0xbb;
  std::atomic<int> amo;

  amo.store (oldval);

  long longval = val_without_const_folding (0xff80000000000000 + oldval);
  oldval = static_cast<int> (longval);

  amo.compare_exchange_strong (oldval, newval);

  if (newval != amo.load (std::memory_order_relaxed))
    __builtin_abort ();

  return 0;
}

