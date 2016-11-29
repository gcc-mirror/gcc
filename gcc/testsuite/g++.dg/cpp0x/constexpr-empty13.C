// { dg-do compile { target c++11 } }

struct A {
  struct B { } b;
} a;
constexpr int f (A a) { return 42; }
constexpr int i = f(a);
