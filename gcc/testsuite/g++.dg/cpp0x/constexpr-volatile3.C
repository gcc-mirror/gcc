// PR c++/80456
// { dg-do compile { target c++11 } }

struct A {
  static constexpr bool test() noexcept { return true; }

  void f() volatile {
    constexpr bool b = test();
  }
};

void g() {
  A a;
  a.f();
}
