// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 3 Jan 2000 <nathan@acm.org>

// Derived from a bug report by Ulrich Drepper <drepper@cygnus.com>

struct A {operator char * () { return 0;} };
struct B {operator char * () const { return 0;} };
struct C {operator char const * () { return 0;} };
struct D {operator char const * () const { return 0;} };

void f0 ()
{
  A a = A ();
  B b = B ();
  C c = C ();
  D d = D ();
  
  static_cast <char *> (a);
  static_cast <char *> (b);
  static_cast <char *> (c);   // ERROR - static cast
  static_cast <char *> (d);   // ERROR - static cast
}

void f1 ()
{
  A a = A ();
  B b = B ();
  C c = C ();
  D d = D ();
  
  static_cast <const char *> (a);
  static_cast <const char *> (b);
  static_cast <const char *> (c);
  static_cast <const char *> (d);
}

void f2 ()
{
  A const a = A ();
  B const b = B ();
  C const c = C ();
  D const d = D ();
  
  static_cast <char *> (a);   // ERROR - static cast
  static_cast <char *> (b);
  static_cast <char *> (c);   // ERROR - static cast
  static_cast <char *> (d);   // ERROR - static cast
}

void f3 ()
{
  A const a = A ();
  B const b = B ();
  C const c = C ();
  D const d = D ();
  
  static_cast <const char *> (a); // ERROR - static cast
  static_cast <const char *> (b);
  static_cast <const char *> (c); // ERROR - static cast
  static_cast <const char *> (d);
}
