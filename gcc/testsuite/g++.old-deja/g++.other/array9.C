// { dg-do assemble  }

template <class T> class A {
  T *d;
  public: void f() { (T[10])d; }
};
