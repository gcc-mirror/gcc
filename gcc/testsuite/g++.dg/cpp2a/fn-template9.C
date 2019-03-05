// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

namespace N1 {
  struct S {};
  template<int X> void f(S);
}

namespace N2 {
  template<class T> void f(T t);
}

void
g (N1::S s)
{
  f<3>(s);

  using N2::f;
  f<3>(s);
}
