// Build don't link:
// 
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Sep 1999 <nathan@acm.org>

int j();
struct B {};
struct A
{
  friend explicit int j();    // ERROR - only ctor decls can be explicit
  friend explicit B::B ();    // ERROR - only ctor decls can be explicit
  int f(const);               // ERROR - ansi forbids no type
  const k;                    // ERROR - ansi forbids no type
  mutable friend int j1 ();   // ERROR - non-member cannot be mutable
  mutable typedef int d;      // ERROR - non-object cannot be mutable
  mutable int fn ();          // ERROR - non-object cannot be mutable
  void fn (mutable int);      // ERROR - non-member cannot be mutable
  mutable static int s;       // ERROR - static cannot be mutable
  mutable const int s1;       // ERROR - const cannot be mutable
  mutable const int *s2;      // ok
  mutable int *const s3;      // ERROR - const cannot be mutable
  explicit A ();              // ok
};
mutable int g;                // ERROR - non-member cannot be mutable
explicit A::A () {}           // ERROR - only ctor decls can be explicit

