// PR c++/72457
// { dg-do compile { target c++11 } }

struct A {
  int i;
  constexpr A(): i(0) {}
};

struct B: A { };

struct C
{
  B b;
  constexpr C() : b{} {}
};
