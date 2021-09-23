// PR c++/102050
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct A {
  A(std::initializer_list<int> = {});
};

A x{0};
A y{1, 2, 3};
A z;
