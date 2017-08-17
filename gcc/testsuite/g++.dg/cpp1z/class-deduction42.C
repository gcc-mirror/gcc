// { dg-options -std=c++1z }

#include <initializer_list>

template <class,class> struct same;
template <class T> struct same<T,T> { };

template <class T>
struct A
{
  A(const A&);
  A(std::initializer_list<T>);
};

A a { 1 };
A b { a };

same<decltype (a), decltype (b)> s;

