// Bug: g++ doesn't see that A is a vbase of C.
// Submitted by Jason Merrill <jason@cygnus.com>
// Build don't link:

struct A {
  int i;
  void f ();
};

struct B: public A { };
struct C: public virtual B { };

void g ()
{
  int C::*p = &A::i;		// ERROR - conversion from vbase
  void (C::*fp)() = &A::f;	// ERROR - conversion from vbase
}
