// { dg-options -std=c++17 }

#include <initializer_list>

struct B { };

template <class T>
struct A
{
  A(std::initializer_list<T>);
  A(T, B);
};

A a { 1, B() };

template <class,class> struct same;
template <class T> struct same<T,T> { };

same<decltype(a), A<int>> s;
