// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++20 } }

struct E {
  constexpr E() = default;
  constexpr E(int) {}
};

struct W {
  [[no_unique_address]] E e;
  constexpr W(int) : e(8) {}
};

constexpr W w = W(42);

struct S {
  E e;
  constexpr S() : e{} { }
};

constexpr S s;

struct S2 {
  [[no_unique_address]] E e;
  constexpr S2() : e{} { }
};

constexpr S2 s2;
