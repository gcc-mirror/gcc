// { dg-do assemble  }
// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Aug 1999 <nathan@acm.org>

// We should spot all ambiguities

struct A {int m;};
struct B : A { int m; };
struct C : A { int m; };
struct D0 : virtual B, virtual C { int m; };
struct D1 : virtual B, C { int m; };
struct D2 : B, virtual C { int m; };
struct D3 : B, C { int m; };

void fn(D0 *d0, D1 *d1, D2 *d2, D3 *d3)
{
  A *a0 = d0;   // { dg-error "" } A is an ambiguous base
  A *a1 = d1;   // { dg-error "" } A is an ambiguous base
  A *a2 = d2;   // { dg-error "" } A is an ambiguous base
  A *a3 = d3;   // { dg-error "" } A is an ambiguous base
}
