// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Jan 2001 <nathan@codesourcery.com>

// Bug 1038. Default args on class members can produce circular dependencies.
// Make sure we spot them, and don't depend on a particular ordering.

struct A
{
  static int Foo (int = Baz ());    // WARNING - circular
  static int Baz (int = Foo ());    // WARNING - circular
};

struct Test
{
  Test (void * = 0);
  void set (const Test &arg = Test ());
};

struct B
{
  static int Bar (int = Foo (1));
  static int Foo (int = Baz ());
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
