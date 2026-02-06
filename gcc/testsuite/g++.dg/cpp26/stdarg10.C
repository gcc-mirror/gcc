// PR c++/123977
// { dg-do compile }

typedef void R;
struct A {
  R foo (...) const volatile;
  void foo ();
};

template <class T>
struct B {
  static void bar () { (T (A::*)) &A::foo; }
};

void
baz ()
{
  B <R (...) const volatile>::bar;
}
