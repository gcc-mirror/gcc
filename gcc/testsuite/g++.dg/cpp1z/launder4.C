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
  int& x;
};

struct B
{
  A a;
};

int
main ()
{
  int x = 42;
  B b{{x}};
  int y = 666;
  new (&b.a) A{y};
  assert(std::launder(&b.a)->x == 666);
}
