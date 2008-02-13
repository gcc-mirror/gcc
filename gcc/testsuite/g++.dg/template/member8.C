// PR c++/35138
// { dg-do compile }

namespace N1 { struct A { }; }
namespace N2 { struct A { }; }
using namespace N1;
using namespace N2;

template <typename T> int
foo (T const &t)
{
  return t.A;
}

struct B
{
  int A;
};

int
main ()
{
  B b;
  foo (b);
}
