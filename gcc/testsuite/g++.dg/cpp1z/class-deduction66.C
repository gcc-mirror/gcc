// PR c++/90173
// { dg-do compile { target c++17 } }

template <typename T> struct A { };

A(int) -> A<int>;

namespace decl {
  A (*fp)() = 0;  // { dg-error "placeholder" }
}
