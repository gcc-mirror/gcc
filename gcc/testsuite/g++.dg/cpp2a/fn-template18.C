// PR c++/88184
// { dg-do compile }
// { dg-options "-std=c++2a -fchecking=2" }

namespace A
{
  void f ();
  void f (int);
  void f (int, int);
}

using A::f;

template <typename T> void g ()
{
  f<T> (); // { dg-error "no matching function for call" }
}

void
fn ()
{
  g<int>();
}
