// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Nov 2000 <nathan@codesourcery.com>

// Bug 611. We ICEd when calling a member function returning an incomplete
// type by value.

struct X;   // { dg-error "" } forward ref

struct Y
{
  X foo ();
};

void baz (Y *p)
{
  p->foo ();    // { dg-error "" } incomplete
}
