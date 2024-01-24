// PR c++/113529
// { dg-do compile { target c++20 } }

#include <compare>

struct A {
  auto operator<=>(const A&) const = default;
  bool operator<(const A&) const;
};
struct B {
  auto operator<=>(const B&) const = default;
};
struct C : A, B { };


template<class T>
void f(T u, T v) {
  static_assert(!requires { u < v; });
  u < v; // { dg-error "request for member 'operator<=>' is ambiguous" }
}

template void f(C, C);
