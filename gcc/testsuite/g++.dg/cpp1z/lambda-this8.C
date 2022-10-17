// PR c++/100493
// { dg-do compile { target c++17 } }
// { dg-options "" }

struct A {
  void f() {
    [=, this] { };
  }
};
