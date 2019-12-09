// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++2a } }

struct E {
  constexpr E() = default;
  constexpr E(int) {}
};

struct W {
  [[no_unique_address]] E e;
  int i;
  constexpr W(int) : e(8), i(11) {}
};

constexpr W w = W(42);

struct S {
  E e;
  int i;
  constexpr S() : e{}, i(11) { }
};

constexpr S s;

struct S2 {
  [[no_unique_address]] E e;
  int i;
  constexpr S2() : e{}, i(11) { }
};

constexpr S2 s2;
