// { dg-do assemble  }
// Bug: forward reference to friend doesn't work in template.

template <class T> class A {
  static int i;
  friend struct B;
};

struct B {
  void f () { A<int>::i = 0; }
};
