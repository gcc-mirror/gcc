// Bug: g++ doesn't instantiate function templates in instantiate_type.
// Build don't link:

template <class T> void fn (T t) { }
template <class T> struct A {
  void (*p)(T);
  A() { p = fn; }
};

A<int> a;
