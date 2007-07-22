// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2007 <nathan@codesourcery.com>

// PR 32839.  Default arguments propagated through the type system to
// an indirect call.

void Quux (int i = 0);
void Baz (int i);

void Foo ()
{
  __typeof (Quux) *q = Baz;

  q (); // { dg-error "too few arguments" }
}

  
