// PR c++/84536
// { dg-do compile { target c++14 } }

template<int... N> auto foo(N...);  // { dg-error "initializer" }

void bar()
{
  foo<>();
}
