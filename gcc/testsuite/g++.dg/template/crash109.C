// PR c++/51328

template<typename T> struct A
{
  void foo(A<T::~T>);  // { dg-error "invalid use of destructor" }
};
