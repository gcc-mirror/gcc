// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Nov 2000 <nathan@codesourcery.com>

// We rejected using decls bringing in functions from a base which would hide a
// nested class of the same name, but only if we had no functions by that name
// already. Also, we failed to find that using declaration during lookup. Also
// we failed to reject using declarations which matched the constructor name.

struct A
{
  int f ();
  void D ();
};

struct A2 {
  typedef int f;
};

struct B : A 
{
  using A::f;
  struct f {};
};

struct C : A 
{
  using A::f;
  int f (int);
  struct f {};
};

void foo (B *bp, C* cp)
{
  bp->f ();
  cp->f ();
}

struct D : A
{
  using A::D;   // { dg-error "" } names constructor
};
