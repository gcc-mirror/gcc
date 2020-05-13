// PR c++/93803 - ICE with constexpr init and [[no_unique_address]].
// { dg-do compile { target c++20 } }

struct empty { };

struct foo {
  [[no_unique_address]] empty x;
  constexpr foo() : x{} { }
};

struct bar : foo {
  using foo::foo;
};

constexpr bar a{};
