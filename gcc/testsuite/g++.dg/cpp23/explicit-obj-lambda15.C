// PR c++/119038
// { dg-do compile { target c++23 } }

struct A {
  void f() {
    [&](auto x) { g(x); h(x); }(0);
  }

  void g(this A&, int);
  void h(this auto&, auto);
};
