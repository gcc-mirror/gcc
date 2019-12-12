// PR c++/77739
// { dg-do compile { target c++14 } }

struct A {
  A();
  A(const A &);
};
struct B {
  B();
  template <typename... Args> auto g(Args &&... p1) {
    return [=] { f(p1...); }; // { dg-warning "implicit capture" "" { target c++2a } }
  }
  void f(A, const char *);
};
B::B() { g(A(), ""); }
