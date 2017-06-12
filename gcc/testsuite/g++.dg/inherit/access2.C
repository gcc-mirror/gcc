// Test that a base doesn't get special rights to convert to itself.

struct A {
  void f ();
};

struct B: private A { };

B b;

void A::f ()
{
  A* ap = &b;			// { dg-error "base|inherit" }
}
