// PR c++/109868
// { dg-do compile }
// { dg-options "-O2" }

struct A { virtual void foo (); };
struct B { long b; int : 0; };
struct C : A { B c; };

void
bar (C *p)
{
  *p = C ();
}
