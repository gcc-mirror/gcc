// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 19 Jan 2001 <nathan@codesourcery.com>

// Bug 1638. We failed to check if a function instantiation produced a void
// parameter type.

template <class T> struct S
{
  int f (T);    // ERROR - void type
};

void foo ()
{
  S<void> s;
}
