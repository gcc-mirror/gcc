// PR c++/64520
// { dg-do compile { target c++11 } }

#include <initializer_list>
struct A {
  template <typename... B> A(std::initializer_list<B...>);
};
A a { 0 };			// { dg-error "" }
