// Bug: g++ groups ->* before casts.
// PRMS Id: 4484 (bug 4)
// Build don't link:

struct A { };
struct B : public A { void f (); };

void g ()
{
  A* ap = new B;
  void (B::*p)() = &B::f;

  ((B*)ap->*p)();		// gets bogus error - incorrect precedence
}
