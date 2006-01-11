// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Jul 2003 <nathan@codesourcery.com>

// Packed fields are unsuitable for direct reference binding.

struct Unpacked { int i; };

void Ref (int &p);
void Ref (Unpacked &p);

struct  __attribute__ ((packed)) Packed
{
  char c;
  int i;
  Unpacked u;
};

void Foo (Packed &p)
{
  Ref (p.i); // { dg-error "cannot bind packed field" "" { target { ! default_packed } } }
  Ref (p.u.i); // { dg-error "cannot bind packed field" "" { target { ! default_packed } } }
  Ref (p.u); // { dg-error "cannot bind packed field" "" { target { ! default_packed } } }
}
