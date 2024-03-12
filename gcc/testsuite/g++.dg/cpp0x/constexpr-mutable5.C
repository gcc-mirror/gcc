// PR c++/109745
// { dg-do run { target c++11 } }
// { dg-additional-options "-O" }

struct A {
  mutable int m = 0;
  void f() const { ++m; };
  constexpr int get_m() const { return m; }
};

struct B { A a; };

struct C { B b; };

int main() {
  constexpr A a;
  a.m++;
  if (a.get_m() != 1 || a.m != 1)
    __builtin_abort();
  a.m++;
  if (a.get_m() != 2 || a.m != 2)
    __builtin_abort();

  constexpr B b;
  b.a.m++;
  if (b.a.get_m() != 1 || b.a.m != 1)
    __builtin_abort();
  b.a.m++;
  if (b.a.get_m() != 2 || b.a.m != 2)
    __builtin_abort();

  constexpr C c;
  c.b.a.m++;
  if (c.b.a.get_m() != 1 || c.b.a.m != 1)
    __builtin_abort();
  c.b.a.m++;
  if (c.b.a.get_m() != 2 || c.b.a.m != 2)
    __builtin_abort();
}
