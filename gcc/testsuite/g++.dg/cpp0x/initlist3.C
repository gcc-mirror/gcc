// { dg-options "-std=c++0x" }

#include <initializer_list>

template <class T> void f(std::initializer_list<T>);

void g()
{
  f({1,2,3});
}

