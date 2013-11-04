// PR c++/43028
// { dg-options "-std=c++11" }

#include <initializer_list>

struct string { string(std::initializer_list<char>) { } };

void f() {
  auto y =
  {
    string(Equation()) // { dg-error "not declared" }
  }; // { dg-error "unable to deduce" }
}
