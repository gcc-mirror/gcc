// PR c++/35650
// { dg-do compile }

void f1 ();

namespace N
{
  using::f1;
  void f2 ();
  void f3 ();
}

using N::f3;

void
test ()
{
  void (&a) () = f1;
  void (&b) () = N::f1;
  void (&c) () = N::f2;
  void (&d) () = f3;
  void (&e) () = ::f3;
}
