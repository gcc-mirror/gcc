// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Jan 2001 <nathan@codesourcery.com>

// Bug 1630. Template deduction at a call allowed conversions more lenient than
// qualification conversions. That would lead to misleading diagnostics during
// overload resolution.


template <typename T> void Foo (T const **);
template <typename T> void Bar (T const * const *);
void Foo (int);       // ERROR - candidate
void Foo (float);     // ERROR - candidate

void baz (int **p1)
{
  Foo (p1);   // ERROR - no such function
  Bar (p1);   // OK
}
