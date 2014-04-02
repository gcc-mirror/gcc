// { dg-do compile { target c++11 } }

namespace N
{
  template <class T> using U = T*;
}

void f(N::U<int>) { blah; } // { dg-error "void f(N::U<int>)|not declared" }
