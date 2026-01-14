// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test invalid reflections in member init lists.

using info = decltype(^^void);

struct A {
  info i;
  constexpr A() : i{^^void} {}  // { dg-error "consteval-only" }
};

struct B {
  info i;
  B() : i{^^void} {}  // { dg-error "consteval-only" }
};

struct C {
  info i;
  constexpr C() : i{} {}  // { dg-error "function of consteval-only type must be declared .consteval." }
};

struct D {
  info i;
  D() : i{} {}  // { dg-error "function of consteval-only type must be declared .consteval." }
};
