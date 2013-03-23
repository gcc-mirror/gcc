// PR c++/56646
// { dg-require-effective-target c++11 }

struct A {
  void f();
};

void A::f() {
  struct B {
    auto g() -> void { }
  };
}
