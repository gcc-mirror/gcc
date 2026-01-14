// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test member list initializers.

using info = decltype(^^int);

struct A {
  info i;
  consteval A() : i{^^void} {}
};

A a1;  // { dg-error "consteval-only variable .a1." }
constinit A a2;
constexpr A a3;

struct B {
 info i;
 info j;
 consteval B() : i{}, j{i} {}
};

B b1;  // { dg-error "consteval-only variable .b1." }
constinit B b2;
constexpr B b3;
