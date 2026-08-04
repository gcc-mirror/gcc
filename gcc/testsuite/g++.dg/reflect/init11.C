// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

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
  constexpr C() : i{} {}
};

struct D {
  info i;
  D() : i{} {}
};
