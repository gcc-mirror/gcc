// Build don't link:
// Special g++ options: -pedantic-errors -ansi -w

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Jan 2001 <nathan@codesourcery.com>

// Bug 1038. Default args on class members can produce circular dependencies.
// Make sure we spot them, and don't depend on a particular ordering.

struct AA
{
  static int Foo (int = Baz ());    // ERROR - candidate
  static int Baz (int = Foo ());    // ERROR - candidate
};

int main ()
{
  AA::Foo ();   // ERROR - no candidate
  AA::Foo (1);
  AA::Baz ();   // ERROR - no candidate
  AA::Baz (1);
  
  return 0;
}
