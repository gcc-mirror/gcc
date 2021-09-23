// PR c++/99859
// { dg-do compile { target c++14 } }

struct A
{
  int i;
  constexpr int f() { return i; }
  constexpr A() : i(0) { i = f(); i = 1; i = f(); }
};

constexpr A a = A();
static_assert (a.i == 1, "");
