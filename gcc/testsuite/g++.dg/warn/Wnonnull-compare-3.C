// PR c++/69850
// { dg-do compile }
// { dg-options "-Wnonnull-compare" }

template <typename T>
struct A {
  static void foo (T *x) { x->bar (); }
};
template <typename T>
struct B {
  T b;
  void operator= (B) { A<T>::foo (&b); }
};
struct C {
  void bar () { delete[] this; }	// { dg-bogus "nonnull argument" }
};
struct D { B<C> d; };
struct G {
  D g[6];
  void baz ();
};
int a;

void
G::baz ()
{
  g[a] = g[1];
}
