// { dg-do assemble  }
// Bug: g++ doesn't instantiate function templates in instantiate_type.

template <class T> void fn (T t) { }
template <class T> struct A {
  void (*p)(T);
  A() { p = fn; }
};

A<int> a;
