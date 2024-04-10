// PR rtl-optimization/113617
// { dg-do link { target { c++17 && c++14_down } } }

#include "pr113617.h"

void qux() {
  A<long long> a;
  a.foo(0, 0);
}

namespace R {
template<>
Y<N1::N2::N3::X<0> >::AI
Y<N1::N2::N3::X<0> >::operator->()
{
  return AI();
}
template<>
Y<N1::N2::N3::X<1> >::AI
Y<N1::N2::N3::X<1> >::operator->()
{
  return AI();
}
}

N1::N2::N3::AB ab;

N1::N2::N3::AB &
N1::N2::N3::AB::bleh()
{
  return ab;
}

N1::N2::N3::AC::AC(int)
{
}

void
N1::N2::N3::AC::m1(R::S<void()>)
{
}

#ifndef SHARED
int
main()
{
}
#endif
