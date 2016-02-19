// PR c++/69850
// { dg-do compile }
// { dg-options "-Wnonnull-compare" }

struct D {
  virtual ~D ();
  void foo () const { delete this; }	// { dg-bogus "nonnull argument" }
  template <typename> friend struct A;
};
template <typename T> struct A {
  static void bar (T *x) { x->foo (); }
};
template <typename T> struct B {
  T b;
  void baz () { A<T>::bar (&b); }
};
class C {
  class E : public D { ~E (); };
  void baz ();
  B<E> c;
};

void
C::baz ()
{
  c.baz ();
}
