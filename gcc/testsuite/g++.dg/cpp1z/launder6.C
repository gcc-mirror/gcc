// { dg-do run { target c++11 } }
// { dg-additional-options "-O2" }
// { dg-additional-sources "launder6.cc" }
#include <cassert>
#include "launder6.h"

namespace std
{
  template <typename T>
  T *
  launder (T *p)
  {
    return __builtin_launder (p);
  }
}

int
main ()
{
  int x = 42;
  B b{{x}};
  f(b);
  assert(std::launder(&b.a)->x == 666);
}
