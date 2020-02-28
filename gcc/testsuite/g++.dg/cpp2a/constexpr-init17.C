// PR c++/93803 - ICE with constexpr init and [[no_unique_address]].
// { dg-do compile { target c++2a } }

struct empty { };

struct foo {
  [[no_unique_address]] empty x, x2, x3;
  constexpr foo() : x{}, x2{} { }
};

struct bar : foo {
  using foo::foo;
};

constexpr bar a{};
