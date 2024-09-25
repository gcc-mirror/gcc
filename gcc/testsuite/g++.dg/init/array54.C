// PR c++/90947
// { dg-do run { target c++11 } }
// { dg-require-effective-target thread_fence }
// { dg-require-effective-target sync_int_long }

#include <atomic>

static std::atomic<int> a[1] { {1} };

int
main ()
{
  if (a[0].load () != 1)
    __builtin_abort ();
}
