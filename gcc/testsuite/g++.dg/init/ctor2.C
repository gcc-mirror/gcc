// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Mar 2003 <nathan@codesourcery.com>

// PR 9629. The vtable is not set up until the base initializers have
// run.

struct A {
  static A *a;
  A ();
};
A *A::a;
A::A () {a = this;}

struct B {
  static A *a;
  B (A *);
};
A *B::a;
B::B(A *a_) {a = a_;}

struct C : virtual public A, public B {
  C();
};
C::C () : B(this) {}

struct D : virtual public C {};

int main()
{
  new D();
  return A::a != B::a;
}
