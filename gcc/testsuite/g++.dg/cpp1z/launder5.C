// { dg-do run { target c++11 } }
// { dg-additional-options "-O2" }
// { dg-additional-sources "launder5.cc" }

#include <cassert>
#include "launder5.h"

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
  B b{{42}};
  f(b);
  assert(std::launder(&b.a)->x == 666);
}
