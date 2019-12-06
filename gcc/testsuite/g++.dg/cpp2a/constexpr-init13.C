// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++2a } }

struct E { };

struct S {
  E e;
  constexpr S() {}
};

constexpr S s;
constexpr S s2[4];

struct W {
  [[no_unique_address]] E e1, e2;
  constexpr W() {}
};

constexpr W w;
constexpr W w2[4];

struct Y {
  [[no_unique_address]] E e;
  __extension__ char a[0];
  constexpr Y() {}
};

constexpr Y y;
constexpr Y y2[4];

struct Z {
  [[no_unique_address]] E e;
  int i;
  constexpr Z(int n) :i(n) { }
};

constexpr Z z(42);
