// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant.

#include <meta>
using namespace std::meta;

// Not copy-constructible.
struct S {
  S();
  S(const S&) = delete;
};

constexpr info r1 = reflect_constant (S{}); // { dg-error "no matching function for call" }

// Not copy-constructible.
struct S2 {
  S2();
  S2(S2&) = delete;
};

constexpr info r2 = reflect_constant (S2{}); // { dg-error "no matching function for call" }

volatile constexpr int vi = 42;
constexpr info r3 = reflect_constant (vi); // { dg-error "lvalue-to-rvalue conversion" }

// Not a structural type.
struct M {
  mutable int i;
};
constexpr info r4 = reflect_constant (M{42}); // { dg-error ".M. must be a cv-unqualified structural type" }

// { dg-error "use of deleted function" "" { target *-*-* } 0 }
