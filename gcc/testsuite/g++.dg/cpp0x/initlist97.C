// PR c++/77752
// { dg-do compile { target c++11 } }

namespace std {
  class initializer_list;  // { dg-error "declaration" }
}
void f(std::initializer_list l) { f({2}); }  // { dg-error "incomplete type" }
