// PR c++/101717
// { dg-do compile { target c++14 } }

struct x {
  static void f() { }
  void (*_)() = [] { [=](auto) { f(); }(0); };
};
