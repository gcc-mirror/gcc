// { dg-do assemble  }

// Copyright (C) 2000, 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Jan 2001 <nathan@codesourcery.com>

// As of G++ 3.4, we no longer attempt to detect dependencies; the
// standard does not require that we do.

// Bug 1038. Default args on class members can produce circular dependencies.
// Make sure we spot them, and don't depend on a particular ordering.

struct A
{
  static int Foo (int = Baz ()); // { dg-error "" } 
  static int Baz (int = Foo ());
};

struct Test
{
  Test (void * = 0);
  void set (const Test &arg = Test ());
};

struct B
{
  static int Bar (int = Foo (1));
  static int Foo (int = Baz ()); // { dg-error "" } 
  static int Baz (int = Foo (1));
};

int main ()
{
  Test t;
  t.set ();
  t.set (t);
  B::Bar ();
  B::Bar (1);
  B::Baz ();
  B::Baz (1);
  B::Foo ();
  B::Foo (1);
  
  return 0;
}
