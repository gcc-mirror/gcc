// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Apr 2004 <nathan@codesourcery.com>

void Baz ();

template <typename T> void Foo1 (T *); // #1
template <typename T> void Foo1 (T const *a) {a (1);} // #2

template <typename T> T const *Foo2 (T *);

template <typename T> void Foo3 (T *, T const * = 0); // { dg-message "note" }

void Bar ()
{
  Foo1 (&Baz); // #1

  Foo2 (&Baz);

  Foo3 (&Baz);

  Foo3 (&Baz, &Baz); // { dg-error "no matching function" "" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } 21 }
}
