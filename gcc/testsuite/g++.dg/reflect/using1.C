// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct A {
  int a;
  consteval A(int p) : a(p) {}
};

constexpr auto r = ^^A;

struct B : A {
  using typename [: r :]::A;
};
