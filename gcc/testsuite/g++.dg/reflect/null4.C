// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test null reflection.

using info = decltype(^^int);

struct S {
  info i = {};
};

static_assert (S{}.i == info ());

struct Q {
  info i;
  consteval Q() : i{} {}
};

static_assert (Q{}.i == info ());
