struct S
{
  template <typename T> void operator() (T) {}
};

namespace N
{
  S s;
  struct A {} a;
}

using N::s;

void f () { s(N::a); }

