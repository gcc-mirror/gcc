// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Apr 2004 <nathan@codesourcery.com>

void Baz ();

template <typename T> void Foo1 (T *);
template <typename T> void Foo1 (T const *a) {a (1);} // { dg-error "too many arguments" }

template <typename T> T const *Foo2 (T *);

template <typename T> void Foo3 (T *, T const * = 0);

void Bar ()
{
  Foo1 (&Baz); // { dg-message "required from here" }

  Foo2 (&Baz);

  Foo3 (&Baz);

  Foo3 (&Baz, &Baz);
}
