// { dg-do run  }
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Jan 1999 <nathan@acm.org>

// Make sure objects with mutable members are never placed in a read only
// section.

// All these are POD structs, and hence do not need ctors
struct A { mutable int i; };
struct B { A a; };
struct C { A a[1]; };
struct D { static A const a; };

// all these are static consts and hence naively suitable for a read only
// section. But they contain a mutable, so must be in a writable section.
static int const i = 0;
static A const a = {0};
static B const b = {{0}};
static C const c = {{{0}}};
static A const aa[] = {{0}};
static B const bb[] = {{{0}}};
static C const cc[] = {{{{0}}}};
A const D::a = {0};

int main()
{
  a.i = 05;
  b.a.i = 05;
  c.a[0].i = 05;
  aa[0].i = 05;
  bb[0].a.i = 05;
  cc[0].a[0].i = 05;
  D::a.i = 05;
  
  if(!a.i) return 1;
  if(!b.a.i) return 1;
  if(!c.a[0].i) return 1;
  if(!aa[0].i) return 1;
  if(!bb[0].a.i) return 1;
  if(!cc[0].a[0].i) return 1;
  if(!D::a.i) return 1;

  return 0;
}
