// { dg-options -std=c++17 }

#include <initializer_list>
template <class T>
struct A
{
  A (std::initializer_list<T>);
};

A a{1,2};

