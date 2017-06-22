// PR c++/80973
// { dg-do compile }
// { dg-options "-fsanitize=undefined -std=c++14" }

struct A {
  A();
  A(const A &);
};
struct B {
  B();
  template <typename... Args> auto g(Args &&... p1) {
    return [=] { f(p1...); };
  }
  void f(A, const char *);
};
B::B() { g(A(), ""); }
