// Build don't link:

template <class T> class A {
  T *d;
  public: void f() { (T[10])d; }
};
