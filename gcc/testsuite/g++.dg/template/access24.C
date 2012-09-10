// PR c++/54437

template <void (*P)()> void f();
class A {
  template <class T> static void g();
  template <class T> static void h () { f<g<T> >(); }
  static void i() { h<int>(); }
};
