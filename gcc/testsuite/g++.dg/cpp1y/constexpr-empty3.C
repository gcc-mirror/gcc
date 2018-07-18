// PR c++/80294
// { dg-do compile { target c++14 } }
// { dg-final { scan-assembler-not "static_init" } }

struct A {
  constexpr int f() { A a = *this; return 42; }
};
struct B: A
{
  int i;
  constexpr B(): i(f()) {}
};

B b;
