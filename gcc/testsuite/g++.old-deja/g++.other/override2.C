// { dg-do assemble  }
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Jason Merrill 14 Jun 2001 <jason@redhat.com>

// Test for diagnosis of missing final overrider.

struct A { virtual void f (); };
struct B1: virtual A { virtual void f (); };
struct B2: virtual A { virtual void f (); };
struct C: public B1, public B2 {}; // { dg-error "" } no final overrider
