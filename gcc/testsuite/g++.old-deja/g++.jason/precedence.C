// { dg-do assemble  }
// Bug: g++ groups ->* before casts.
// PRMS Id: 4484 (bug 4)

struct A { };
struct B : public A { void f (); };

void g ()
{
  A* ap = new B;
  void (B::*p)() = &B::f;

  ((B*)ap->*p)();		// { dg-bogus "" } incorrect precedence
}
