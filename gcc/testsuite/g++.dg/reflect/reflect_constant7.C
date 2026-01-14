// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant.

#include <meta>
using namespace std::meta;

struct A {
  int *const p;
  constexpr A(int *p) : p(p) { delete p; }
  constexpr A(const A &) : p(0) {}
};

consteval info f() {
  return reflect_constant<A>(new int);
}

const A &a = [:f():];
