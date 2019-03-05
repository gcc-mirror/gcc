// P0846R0
// { dg-do compile }

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
  f<3>(s); // { dg-error "was not declared" "" { target c++17_down } }
}
