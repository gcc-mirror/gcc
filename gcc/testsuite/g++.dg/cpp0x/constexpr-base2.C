// PR c++/46293
// { dg-do compile { target c++11 } }

struct A
{
};

struct C
{
  int i;
  constexpr C(int i): i(i) {}
};

struct B: A, C
{
  constexpr B(): A(), C(42) { }
};

constexpr B b{};
