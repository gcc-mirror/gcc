// PR c++/106366
// { dg-do compile { target c++17 } }

#include <initializer_list>

template<class T>
struct A { A(...); };

template<typename T>
A(std::initializer_list<T>) -> A<T>;

A a{1,2,3};
using type = decltype(a);
using type = A<int>;
