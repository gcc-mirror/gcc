// PR c++/56639

struct A {
  int i;
  static A* f();
};

struct B {
  void g() {
    int (A::f()->i);
  }
};
