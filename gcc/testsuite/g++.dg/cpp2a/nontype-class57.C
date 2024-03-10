// PR c++/110122
// { dg-do compile { target c++20 } }

struct Foo {
  Foo() = default;
  Foo(const Foo&) = delete;
};

template<Foo V>
struct A {
  A() {
    [] { A a; }();
    [this] { this; }();
  }

  struct B {
    B() {
      [] { A a; }();
      [this] { this; }();
    }
  };
};

A<Foo{}> a;
A<Foo{}>::B b;
