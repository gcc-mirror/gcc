// PR c++/99132
// { dg-do compile { target c++11 } }

template <class T> struct A { T c; };
template <class T> struct B {
  A<T> d;
  constexpr T operator-> () { return d.c; }
  B (B &&);
};
struct C {
  virtual void foo ();
  void bar (B<C *> h) { h->foo (); }
};
