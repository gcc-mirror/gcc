// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++2a } }

struct S {
  constexpr S(int) {}
};

struct W {
  constexpr W(int) : s(8), p() {}

  S s;
  int *p;
};

constexpr auto a = W(42);
