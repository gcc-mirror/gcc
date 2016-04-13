// PR c++/70386
// { dg-options "-Wall" }

struct A { void f () {} };
struct B : public A {};
struct C : public A {};
struct D : public B, public C {};

typedef void (C::*cp) ();
typedef void (D::*dp) ();

int
main ()
{
  cp c = &A::f;
  dp d = c;
  return (cp () == d);
}
