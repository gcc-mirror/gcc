// PR c++/22180

struct A {};

template<typename T> void foo()
{
  T::~T(); // { dg-error "member" }
}

template void foo<A>(); // { dg-message "instantiated" }
