// PR c++/104513
// { dg-do compile { target c++14 } }

struct A {
  int a1;
  short a2, a3;
  long a4;
  constexpr A() : a1(42), a2(42), a3(42), a4(42) { return; }
};
constexpr A a;
