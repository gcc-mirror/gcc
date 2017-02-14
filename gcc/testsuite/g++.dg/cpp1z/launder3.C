// { dg-do run { target c++11 } }
// { dg-additional-options "-O2" }

#include <cassert>

void *
operator new (decltype (sizeof (0)), void *p)
{
  return p;
}

namespace std
{
  template <typename T>
  T *
  launder (T *p)
  {
    return __builtin_launder (p);
  }
}

struct A
{
  const int x;
};

struct B
{
  A a;
};

int
main ()
{
  B b{{42}};
  new (&b.a) A{666};
  assert(std::launder(&b.a)->x == 666);
}
