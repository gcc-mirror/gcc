// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 June 2000 <nathan@codesourcery.com>

// Origin GNATS bug report 69 from Glenn Ammons <ammons@cs.wisc.edu>
// 
// A base which derives a virtual base hides declarations in the virtual base,
// even if that virtual base is accessible via another path [10.2]/6. Make
// sure that non-virtual bases of the virtual base are also hidden, not matter
// what order bases are declared in.

struct A {int a;};
struct B : A {};

struct L1 : virtual B { int a; };
struct L2 : virtual A { int a; };

struct R1 : virtual B {};
struct R2 : virtual A {};

struct C1 : R1, L1 {};
struct C2 : R2, L2 {};

struct D1 : L1, R1 {};
struct D2 : L2, R2 {};

void fn (C1 *c1, D1 *d1, C2 *c2, D2 *d2)
{
  c1->a = 1;
  d1->a = 1;
  c2->a = 1;
  d2->a = 1;
}
