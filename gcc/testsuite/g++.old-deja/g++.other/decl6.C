// { dg-do assemble  }
// 
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Sep 1999 <nathan@acm.org>

int j();
struct B {};
struct A
{
  friend explicit int j();    // { dg-error "" } only ctor decls can be explicit
  friend explicit B::B ();    // { dg-error "" } only ctor decls can be explicit
  int f(const);               // { dg-error "" } ansi forbids no type
  const k;                    // { dg-error "" } ansi forbids no type
  mutable friend int j1 ();   // { dg-error "" } non-member cannot be mutable
  mutable typedef int d;      // { dg-error "" } non-object cannot be mutable
  mutable int fn ();          // { dg-error "" } non-object cannot be mutable
  void fn (mutable int);      // { dg-error "" } non-member cannot be mutable
  mutable static int s;       // { dg-error "" } static cannot be mutable
  mutable const int s1;       // { dg-error "" } const cannot be mutable
  mutable const int *s2;      // ok
  mutable int *const s3;      // { dg-error "" } const cannot be mutable
  explicit A ();              // ok
};
mutable int g;                // { dg-error "" } non-member cannot be mutable
explicit A::A () {}           // { dg-error "" } only ctor decls can be explicit

