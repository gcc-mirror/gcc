// { dg-do compile }
// { dg-options "-pedantic-errors -w" }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Nov 2001 <nathan@nathan@codesourcery.com>

// PR 164
// Although a direct base can be inaccessible due to ambiguity, that
// should not blow up synthesized methods.

struct A {int m;};
struct B : A {int m;};
struct C : virtual A, B {int m;};
struct D : B, C {int m;};

void foo2 ()
{
  D d;
  D e (d);

  e = d;
}
