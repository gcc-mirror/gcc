// PR debug/16261
// { dg-do compile }

namespace N
{
  struct A {};
  typedef A B;
}

void foo()
{
  struct C
  {
    C(N::B) {}
  };

  N::B b;
  C c(b);
}
