// PR c++/69850
// { dg-do compile }
// { dg-options "-Wnonnull-compare" }

struct A { virtual ~A (); int foo (); };
struct B { virtual ~B () { } };
struct C : B, A { };

int
A::foo ()
{
  C *c = dynamic_cast<C *> (this);	// { dg-bogus "nonnull argument" }
  return !c;
}
