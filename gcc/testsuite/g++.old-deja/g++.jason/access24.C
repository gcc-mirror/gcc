// Bug: forward reference to friend doesn't work in template.
// Build don't link:

template <class T> class A {
  static int i;
  friend struct B;
};

struct B {
  void f () { A<int>::i = 0; }
};
