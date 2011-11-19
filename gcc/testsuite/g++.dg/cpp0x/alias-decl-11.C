// { dg-options "-std=c++0x" }

namespace N
{
  template <class T> using U = T*;
};

void f(N::U<int>) { blah; } // { dg-error "void f(N::U<int>)|not declared" }
