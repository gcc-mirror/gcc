// { dg-do assemble  }
// { dg-options "-pedantic-errors -ansi -w" }

// Copyright (C) 2000, 2002, 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Jan 2001 <nathan@codesourcery.com>

// As of G++ 3.4, we no longer attempt to detect dependencies; the
// standard does not require that we do.

// Bug 1038. Default args on class members can produce circular dependencies.
// Make sure we spot them, and don't depend on a particular ordering.

struct AA
{
  static int Foo (int = Baz ()); // { dg-error "" } not yet been parsed
  static int Baz (int = Foo ());
};

int main ()
{
  AA::Foo ();
  AA::Foo (1);
  AA::Baz ();
  AA::Baz (1);
  
  return 0;
}
